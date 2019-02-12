#ifndef __LIB_BATTERY_DISPATCH_TEST_H__
#define __LIB_BATTERY_DISPATCH_TEST_H__

#include <gtest/gtest.h>
#include <lib_util.h>
#include <lib_battery_dispatch.h>
#include <lib_battery_powerflow.h>
#include <lib_power_electronics.h>

#include "lib_battery_properties.h"

/// Structure for battery dispatch test settings
struct DispatchProperties
{

	// Generic dispatch
	int dispatchChoice;
	int currentChoice;
	double currentChargeMax;
	double currentDischargeMax;
	double powerChargeMax;
	double powerDischargeMax;
	double minimumModeTime;
	int meterPosition;
	size_t * schedWeekday;
	util::matrix_t<size_t> scheduleWeekday;
	util::matrix_t<size_t> scheduleWeekend;
	std::vector<bool> canCharge;
	std::vector<bool> canDischarge;
	std::vector<bool> canGridcharge;
	std::map<size_t, double> percentDischarge;
	std::map<size_t, double> percentGridcharge;

	// Front of meter auto dispatch
	std::vector<double> ppaFactors;
	UtilityRate * ur{nullptr};
	util::matrix_t<size_t> ppaWeekend;
	util::matrix_t<size_t> ppaWeekday;


	/// Constructor for dispatch properties
	DispatchProperties()
	{

		// dispatch
		dispatchChoice = 4;
		currentChoice = 0;
		currentChargeMax = 100;
		currentDischargeMax = 100;
		powerChargeMax = 50;
		powerDischargeMax = 50;
		minimumModeTime = 0.1;
		meterPosition = 0;

		schedWeekday = new size_t[24 * 12];

		int i = 0;
		for (int m = 0; m < 12; m++) {
			for (int h = 0; h < 24; h++) {
				schedWeekday[i] = 1;
				if (h > 11 && h < 19) {
					schedWeekday[i] = 3;
				}
				i++;
			}
		}
		scheduleWeekday.assign(schedWeekday, 12, 24);
		scheduleWeekend.assign(schedWeekday, 12, 24);

		for (int p = 0; p < 6; p++) {
			canCharge.push_back(1);
			canDischarge.push_back(1);
			canGridcharge.push_back(0);
			percentDischarge[p] = 100;
		}
		// dispatch FOM
		ppaFactors.push_back(1.0);
		ppaWeekday.resize_fill(12, 24, 1);
		ppaWeekend.resize_fill(12, 24, 1);
	}
	/// Destructor
	~DispatchProperties() {
		if (schedWeekday) {
			delete schedWeekday;
		}
	}
};

/**
* \class BatteryDispatchTest
*
* Test various components of the battery dispatch algorithms
*
*/
class BatteryDispatchTest : public BatteryProperties , public DispatchProperties
{
protected:
	

	capacity_lithium_ion_t * capacityModel;
	voltage_dynamic_t * voltageModel;
	thermal_t * thermalModel;
	lifetime_calendar_t * calendarModel;
	lifetime_cycle_t * cycleModel;
	lifetime_t * lifetimeModel;
	losses_t * lossModel;
	battery_t * batteryModel;
	BatteryPower * batteryPower;

	capacity_lithium_ion_t * capacityModelFOM;
	voltage_dynamic_t * voltageModelFOM;
	losses_t * lossModelFOM;
	battery_t *batteryModelFOM;

	dispatch_manual_t * dispatchManual{nullptr};
	dispatch_automatic_behind_the_meter_t * dispatchAutoBTM{nullptr};
	dispatch_automatic_front_of_meter_t * dispatchAutoFOM{nullptr};

	double P_pv;
	double V_pv;
	double P_load;
	double P_clipped;

	/*! Variables to store forecast data */
	std::vector<double> pv_prediction;
	std::vector<double> load_prediction;
	std::vector<double> cliploss_prediction;

public:

	void SetUp()
	{
		// For Manual Dispatch Test
		BatteryProperties::SetUp();
		capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);
		voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance);
		cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
		calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
		lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
		thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
		lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLosses);
		batteryModel = new battery_t(dtHour, chemistry);
		batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
		dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
			dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

		dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
			currentDischargeMax, powerChargeMax, powerDischargeMax, 0, 0, 0, 1, 24, 1, true, true, false, false);

		// For Debugging Input Battery Target front of meter minute time steps
		double dtHourFOM = 1.0 / 60.0;
		capacityModelFOM = new capacity_lithium_ion_t(2.25 * 444, 63.3475, 95, 15);
		voltageModelFOM = new voltage_dynamic_t(139, 444, 3.6, 4.10, 4.05, 3.4, 2.25, 0.04, 2.00, 0.2, 0.2);
		lossModelFOM = new losses_t(dtHourFOM, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLossesMinute);
		batteryModelFOM = new battery_t(dtHourFOM, chemistry);
		batteryModelFOM->initialize(capacityModelFOM, voltageModelFOM, lifetimeModel, thermalModel, lossModelFOM);
		dispatchAutoFOM = new dispatch_automatic_front_of_meter_t(batteryModelFOM, dtHourFOM, 15, 95, 1, 999, 999, 500, 500, 1, 3, 0, 1, 24, 1, true, true, false, true, 0, 0, 0, 0, ppaFactors, ppaWeekday, ppaWeekend, ur, 98, 98, 98);

		P_pv = P_load = V_pv = P_clipped = 0;		
	}
	void TearDown()
	{
		BatteryProperties::TearDown();
		if (capacityModel) {
			delete capacityModel;
			capacityModel = nullptr;
		}
		if (voltageModel) {
			delete voltageModel;
			voltageModel = nullptr;
		}
		if (voltageModelFOM) {
			delete voltageModelFOM;
			voltageModelFOM = nullptr;
		}
		if (cycleModel) {
			delete cycleModel;
			cycleModel = nullptr;
		}
		if (calendarModel) {
			delete calendarModel;
			calendarModel = nullptr;
		}
		if (capacityModelFOM) {
			delete capacityModelFOM;
			capacityModelFOM = nullptr;
		}
		if (lifetimeModel) {
			delete lifetimeModel;
			lifetimeModel = nullptr;
		}
		if (thermalModel) {
			delete thermalModel;
			thermalModel = nullptr;
		}
		if (lossModel) {
			delete lossModel;
			lossModel = nullptr;
		}
		if (lossModelFOM) {
			delete lossModelFOM;
			lossModelFOM = nullptr;
		}
		if (batteryModel) {
			delete batteryModel;
			batteryModel = nullptr;
		}
		if (batteryModelFOM) {
			delete batteryModelFOM;
			batteryModelFOM = nullptr;
		}
		if (dispatchManual) {
			delete dispatchManual;
			dispatchManual = nullptr;
		}
		if (dispatchAutoBTM) {
			delete dispatchAutoBTM;
			dispatchAutoBTM = nullptr;
		}
		if (dispatchAutoFOM) {
			delete dispatchAutoFOM;
			dispatchAutoFOM = nullptr;
		}
	}

};



#endif
