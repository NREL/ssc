#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_OPS_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_OPS_H

struct storage_time
{
    /* Changing values */
    size_t year;
    size_t hour;
    size_t step;
    size_t index; // lifetime_index (0 - nyears * steps_per_hour * 8760)
    size_t year_index; // index for one year (0- steps_per_hour * 8760)

    /* Constant values */
    size_t step_per_hour;
    size_t step_per_year;
    size_t nyears;
    size_t total_steps;
    double dt_hour;
};

struct storage_forecast
{
    std::vector<double> pv_prediction;
    std::vector<double> load_prediction;
    std::vector<double> cliploss_prediction;
    int prediction_index;
};

struct storage_state_outputs
{
    double  *totalCharge,
            *availableCharge,
            *boundCharge,
            *maxChargeAtCurrent,
            *maxCharge,
            *maxChargeThermal,
            *SOC,
            *DOD,
            *current,
            *cellVoltage,
            *batteryVoltage,
            *capacityPercent;
};

struct storage_annual_outputs
{
  double *PVChargeEnergy,
          *gridChargeEnergy,
          *chargeEnergy,
          *dischargeEnergy,
          *gridImportEnergy,
          *gridExportEnergy,
          *energySystemLoss,
          *energyLoss;
};

struct storage_lifetime_outputs
{
    double  *DODCycleAverage,
            *cycles,
            *temperature,
            *capacityPercentCycle,
            *capacityPercentCalendar,
            *capacityThermalPercent,
            *replacementsPerYear;
};

struct fuelcell_power_outputs
{
    double  *power,
            *toLoad,
            *toBatt,
            *toGrid;
};

struct battery_power_outputs
{
    double  *power,
            *powerTarget,
            *toLoad,
            *toGrid,
            *fromPV,
            *fromGrid;
};

struct storage_dispatch_outputs
{
    double  *DispatchMode,
            *GenPower,
            *PVToLoad,
            *PVToGrid,
            *GridPower,
            *GridToLoad,
            *GridPowerTarget;

    fuelcell_power_outputs fuelcell;
    battery_power_outputs battery;
};

struct storage_outputs
{
    double  *BatteryConversionPowerLoss,
            *BatterySystemLoss;

    double  *MarketPrice,
            *CostToCycle;

    double averageCycleEfficiency;
    double averageRoundtripEfficiency;

    double PVChargePercent;

    storage_state_outputs charge;
    storage_annual_outputs annual;
    storage_lifetime_outputs lifetime;

    storage_dispatch_outputs power;
};

#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_OPS_H
