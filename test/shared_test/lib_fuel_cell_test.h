#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include <lib_fuel_cell.h>
#include <lib_fuel_cell_dispatch.h>
#include <lib_util.h>


class FuelCellProperties : public ::testing::Test
{
protected:

	size_t numberOfUnits;
	double unitPowerMax_kW;
	double unitPowerMin_kW;
	double startup_hours;
	double dynamicResponse_kWperHour;
	double degradation_kWperHour;
	double degradationRestart_kW;
	double replacement_percent;
	util::matrix_t<double> efficiencyTable;
	double lowerHeatingValue_BtuPerFt3;
	double higherHeatingValue_BtuPerFt3;
	double availableFuel_Mcf;
	int shutdownOption;
	int dispatchOption;
	double dt_hour;
	double fixed_percent;

	std::vector<double> dispatchInput_kW;
	std::vector<bool> canCharge;
	std::vector<bool> canDischarge;
	std::map<size_t, double> discharge_percent;
	util::matrix_t<size_t> scheduleWeekday;
	util::matrix_t<size_t> scheduleWeekend;


	void SetUp()
	{
		numberOfUnits = 1;
		unitPowerMax_kW = 100;
		unitPowerMin_kW = 20;
		startup_hours = 8;
		dynamicResponse_kWperHour = 20;
		degradation_kWperHour = 0.01;
		degradationRestart_kW = 5;
		replacement_percent = 50;
		lowerHeatingValue_BtuPerFt3 = 1033;
		higherHeatingValue_BtuPerFt3 = 1033;
		availableFuel_Mcf = 10000;
		shutdownOption = FuelCell::FC_SHUTDOWN_OPTION::IDLE;
		dispatchOption = FuelCellDispatch::FC_DISPATCH_OPTION::FIXED;
		dt_hour = 1.0;
		fixed_percent = 40;

		const double tmpValues[33] = { 0,0,50,16,21,50,25,25,50,34,32,50,44,37,50,53,42,50,62,47,49,72,50,48,82,52,47,90,52,46,100,51,45 };
		efficiencyTable.assign(tmpValues, 11, 3);

		canCharge.push_back(1);
		canDischarge.push_back(1);
		discharge_percent[1] = 40;
		scheduleWeekday.resize_fill(12, 24, 1);
		scheduleWeekend.resize_fill(12, 24, 1);

		for (size_t t = 0; t < 8760; t++) {
			dispatchInput_kW.push_back(50);
		}
		
	}
};

/**
* \class FuelCellTest
*
* This class contains the setup and teardown structure required to test the fuel cell model
*
*/
class FuelCellTest : public FuelCellProperties
{
protected:

	FuelCell * fuelCell;
	FuelCellDispatch * fuelCellDispatch;
	
	

public:

	void SetUp()
	{
		FuelCellProperties::SetUp();
		fuelCell = new FuelCell(unitPowerMax_kW, unitPowerMin_kW, startup_hours, dynamicResponse_kWperHour, degradation_kWperHour, degradationRestart_kW,
			replacement_percent, efficiencyTable, lowerHeatingValue_BtuPerFt3, higherHeatingValue_BtuPerFt3, availableFuel_Mcf, shutdownOption, dt_hour);
		fuelCellDispatch = new FuelCellDispatch(fuelCell, numberOfUnits, dispatchOption, shutdownOption, dt_hour, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, scheduleWeekday, scheduleWeekend);

	}
	void TearDown()
	{
		if (fuelCell) {
			delete fuelCell;
		}
		if (fuelCellDispatch) {
			delete fuelCellDispatch;
		}
	}

};



#endif