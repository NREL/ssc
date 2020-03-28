#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H

#include <gtest/gtest.h>
#include "lib_battery_dispatch_test.h"


/**
* \class BatteryDispatchTest
*
* Test automatic front of the meter, AC-Coupled
*
*/
class AutoFOMTest_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:
    thermal_t * thermalModel;
    lifetime_calendar_t * calendarModel;
    lifetime_cycle_t * cycleModel;
    lifetime_t * lifetimeModel;
    BatteryPower * batteryPower;

    capacity_lithium_ion_t * capacityModelFOM;
    voltage_dynamic_t * voltageModelFOM;
    losses_t * lossModelFOM;
    battery_t *batteryModelFOM;

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

    void CreateBattery(double dtHour){
        // For Debugging Input Battery Target front of meter minute time steps
        BatteryProperties::SetUp();

        capacityModelFOM = new capacity_lithium_ion_t(2.25 * 444, 63.3475, 95, 15);
        voltageModelFOM = new voltage_dynamic_t(139, 444, 3.6, 4.10, 4.05, 3.4, 2.25, 0.04, 2.00, 0.2, 0.2, dtHour);
        cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
        calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
        lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
        thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
        lossModelFOM = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModelFOM, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLossesMinute);
        batteryModelFOM = new battery_t(dtHour, chemistry);
        batteryModelFOM->initialize(capacityModelFOM, voltageModelFOM, lifetimeModel, thermalModel, lossModelFOM);

        P_pv = P_load = V_pv = P_clipped = 0;
    }
    void TearDown()
    {
        BatteryProperties::TearDown();
        delete capacityModelFOM;
        delete voltageModelFOM;
        delete cycleModel;
        delete calendarModel;
        delete lifetimeModel;
        delete thermalModel;
        delete lossModelFOM;
        delete batteryModelFOM;
        delete dispatchAutoFOM;
    }

};


/**
* \class BatteryDispatchTest
*
* Automatic front of meter with DC coupling of the battery dispatch algorithms
*
*/
class AutoFOMDC_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:
    thermal_t * thermalModel;
    lifetime_calendar_t * calendarModel;
    lifetime_cycle_t * cycleModel;
    lifetime_t * lifetimeModel;
    BatteryPower * batteryPower;

    capacity_lithium_ion_t * capacityModelDC;
    voltage_dynamic_t * voltageModelDC;
    losses_t * lossModelDC;
    battery_t *batteryModelDC;

    dispatch_automatic_front_of_meter_t * dispatchAutoDC{ nullptr };

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

        capacityModelDC = new capacity_lithium_ion_t(2.25 * 133227, 50, 100, 10);
        voltageModelDC = new voltage_dynamic_t(139, 133227, 3.6, 4.10, 4.05, 3.4, 2.25, 0.04, 2.00, 0.2, 0.2, dtHour);
        cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
        calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
        lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
        thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
        lossModelDC = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModelDC, lossChoice);
        batteryModelDC = new battery_t(dtHour, chemistry);
        batteryModelDC->initialize(capacityModelDC, voltageModelDC, lifetimeModel, thermalModel, lossModelDC);

        P_pv = P_load = V_pv = P_clipped = 0;

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }
    void TearDown()
    {
        BatteryProperties::TearDown();
        delete capacityModelDC;
        delete voltageModelDC;
        delete cycleModel;
        delete calendarModel;
        delete lifetimeModel;
        delete thermalModel;
        delete lossModelDC;
        delete batteryModelDC;
        delete dispatchAutoDC;
    }

};



#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
