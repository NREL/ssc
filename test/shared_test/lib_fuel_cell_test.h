#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include <lib_fuel_cell.h>

/**
* \class FuelCellTest
*
* This class contains the setup and teardown structure required to test the fuel cell model
*
*/
class FuelCellTest : public ::testing::Test
{
protected:

	FuelCell * fuelCell;

public:

	void SetUp()
	{
		fuelCell = new FuelCell();
	}
	void TearDown()
	{
		if (fuelCell) {
			delete fuelCell;
		}
	}

};



#endif