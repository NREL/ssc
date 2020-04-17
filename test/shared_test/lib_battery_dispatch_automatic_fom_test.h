#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H

#include <gtest/gtest.h>
#include "lib_battery_dispatch_test.h"


/**
* \class BatteryDispatchTest
*
* Automatic front of meter with AC or DC coupling of the battery dispatch algorithms
*
*/
class AutoFOM_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:
    thermal_t * thermalModel;
    lifetime_calendar_t * calendarModel;
    lifetime_cycle_t * cycleModel;
    lifetime_t * lifetimeModel;
    BatteryPower * batteryPower;

    capacity_lithium_ion_t * capacityModel;
    voltage_dynamic_t * voltageModel;
    losses_t * lossModel;
    battery_t *batteryModel;

    dispatch_automatic_front_of_meter_t * dispatchAuto{nullptr };

    double P_pv;
    double V_pv;
    double P_load;
    double P_clipped;

    /*! Variables to store forecast data */
    std::vector<double> pv_prediction;
    std::vector<double> load_prediction;
    std::vector<double> cliploss_prediction;

public:

    void CreateBattery(double dtHour)
    {
        // For testing Automated Front-of-meter DC-coupled
        BatteryProperties::SetUp();

        capacityModel = new capacity_lithium_ion_t(2.25 * 133227, 50, 100, 10);
        voltageModel = new voltage_dynamic_t(139, 133227, 3.6, 4.10, 4.05, 3.4, 2.25, 0.04, 2.00, 0.2, 0.2, dtHour);
        cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
        calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
        lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
        thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
        lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice);
        batteryModel = new battery_t(dtHour, chemistry);
        batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

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
        delete dispatchAuto;
    }

};



#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
