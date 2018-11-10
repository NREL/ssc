#include <memory>

#include "lib_fuel_cell.h"
#include "lib_fuel_cell_dispatch.h"


FuelCellDispatch::FuelCellDispatch(FuelCell * fuelCell, size_t numberOfUnits, int dispatchOption, int shutdownOption, double dt_hour)
	: m_numberOfUnits(numberOfUnits), m_dispatchOption(dispatchOption), m_shutdownOption(shutdownOption), dt_hour(dt_hour)
{

	m_fuelCellVector.push_back(fuelCell);

	// figure out for multiple fuel cells 
	/*
	for (size_t fc = 1; fc < numberOfUnits; fc++) {
		std::unique_ptr<FuelCell> fuelCelltmp(new FuelCell());
		fuelCelltmp.get() = fuelCell;
	}
	*/

}

