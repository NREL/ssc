#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include <lib_util.h>
#include <lib_battery_dispatch.h>
#include <lib_battery_powerflow.h>
#include <lib_power_electronics.h>

// Generic Lithium-ion battery system to be re-used
class BatteryProperties : public ::testing::Test
{
protected:

	// capacity
	double q;
	double SOC_min;
	double SOC_max;
	double SOC_init;

	// voltage
	int n_series;
	int n_strings;
	double Vnom_default;
	double Vfull;
	double Vexp;
	double Vnom;
	double Qfull;
	double Qexp;
	double Qnom;
	double C_rate;
	double resistance;

	// lifetime
	util::matrix_t<double> cycleLifeMatrix;
	util::matrix_t<double> calendarLifeMatrix;
	int calendarChoice;
	int replacementOption;
	double replacementCapacity;

	// thermal
	double mass;
	double length;
	double width;
	double height;
	double Cp;
	double h;
	double T_room;
	util::matrix_t<double> capacityVsTemperature;

	// losses
	std::vector<double> monthlyLosses;
	int lossChoice;

	// battery
	int chemistry;
	double dtHour;

	// dispatch
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


	void SetUp()
	{
		// capacity
		q = 1000;
		SOC_init = 50;
		SOC_min = 15;
		SOC_max = 95;

		// voltage
		n_series = 139;
		n_strings = 89;
		Vnom_default = 3.6;
		Vfull = 4.1;
		Vexp = 4.05;
		Vnom = 3.4;
		Qfull = 2.25;
		Qexp = 0.04;
		Qnom = 2.0;
		C_rate = 0.2;
		resistance = 0.2;

		// lifetime
		double vals[] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
		cycleLifeMatrix.assign(vals, 6, 3);
		double vals2[] = {0, 100, 3650, 80, 7300, 50};
		calendarLifeMatrix.assign(vals, 3, 2);
		calendarChoice = 1;
		replacementOption = 0;

		// thermal
		mass = 507;
		length = 0.58;
		width = 0.58;
		height = 0.58;
		Cp = 1004;
		h = 500;
		T_room = 20;
		double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
		capacityVsTemperature.assign(vals3, 4, 2);

		// losses
		for (size_t m = 0; m < 12; m++) {
			monthlyLosses.push_back(0.);
		}
		lossChoice = 0;

		// battery
		chemistry = 1;
		dtHour = 1.0;

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

		
	}
	void TearDown()
	{
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
class BatteryDispatchTest : public BatteryProperties
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

	dispatch_manual_t * dispatchManual;
	dispatch_automatic_behind_the_meter_t * dispatchAutoBTM;
	dispatch_automatic_front_of_meter_t * dispatchAutoFOM;

	double P_pv;
	double V_pv;
	double P_load;
	double P_clipped;

public:

	void SetUp()
	{
		BatteryProperties::SetUp();
		capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);
		voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance);
		cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
		calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
		lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
		thermalModel = new thermal_t(mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
		lossModel = new losses_t(lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, monthlyLosses);
		batteryModel = new battery_t(dtHour, chemistry);
		batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

		P_pv = P_load = V_pv = P_clipped = 0;		
	}
	void TearDown()
	{
		BatteryProperties::TearDown();
		if (capacityModel) {
			delete capacityModel;
		}
		if (voltageModel) {
			delete voltageModel;
		}
		if (cycleModel) {
			delete cycleModel;
		}
		if (calendarModel) {
			delete calendarModel;
		}
		if (lifetimeModel) {
			delete lifetimeModel;
		}
		if (thermalModel) {
			delete thermalModel;
		}
		if (lossModel) {
			delete lossModel;
		}
		if (batteryModel) {
			delete batteryModel;
		}
		if (dispatchManual) {
			delete dispatchManual;
		}
		if (dispatchAutoBTM) {
			delete dispatchAutoBTM;
		}
		if (dispatchAutoFOM) {
			delete dispatchAutoFOM;
		}
	}

};



#endif