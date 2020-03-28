#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H

#include <gtest/gtest.h>
#include "lib_battery_dispatch_test.h"

/**
* \class BatteryDispatchTest
*
* Automatic behind the meter battery dispatch algorithms
*
*/
class AutoBTMTest_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
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

    dispatch_automatic_behind_the_meter_t * dispatchAutoBTM{nullptr};

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
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
        cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
        calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
        lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
        thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
        lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLosses);
        batteryModel = new battery_t(dtHour, chemistry);
        batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                                                    currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, 0, 0, 0, 1, 24, 1, true, true, false, false);

        P_pv = P_load = V_pv = P_clipped = 0;

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }
    void TearDown()
    {
        BatteryProperties::TearDown();
        delete capacityModel;
        delete voltageModel;
        delete cycleModel;
        delete calendarModel;
        delete lifetimeModel;
        delete thermalModel;
        delete lossModel;
        delete batteryModel;
        delete dispatchAutoBTM;
    }
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H
