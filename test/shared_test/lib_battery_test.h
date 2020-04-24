#ifndef __LIB_BATTERY_TEST_H__
#define __LIB_BATTERY_TEST_H__

#include <gtest/gtest.h>

#include <lib_battery.h>

#include "lib_battery_properties.h"

/// Test Battery Model and submodels
class BatteryTest : public BatteryProperties
{
public:

	capacity_lithium_ion_t * capacityModel;
	voltage_dynamic_t * voltageModel;
	thermal_t * thermalModel;
	lifetime_t * lifetimeModel;
	losses_t * lossModel;
	battery_t * batteryModel;

    double max_power = 250;
    double surface_area = 1.2 * 1.2 * 6;
    int n_series = 139;
    int n_strings = 888;

	double dtHour = 1;

	void SetUp()
	{
		BatteryProperties::SetUp();
		capacityModel = new capacity_lithium_ion_t(q * n_strings, SOC_init, SOC_max, SOC_min, 1.0);
		voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
        lifetimeModel = new lifetime_t(cycleLifeMatrix, dtHour, calendarLifeMatrix);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
		lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLosses);
		batteryModel = new battery_t(dtHour, chemistry);
		batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
	}
	void TearDown() {
		if (capacityModel) {
			delete capacityModel;
			capacityModel = nullptr;
		}
		if (voltageModel) {
			delete voltageModel;
			voltageModel = nullptr;
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
		if (batteryModel) {
			delete batteryModel;
			batteryModel = nullptr;
		}
	}
};

#endif
