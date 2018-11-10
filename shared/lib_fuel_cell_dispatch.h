#ifndef _LIB_FUEL_CELL_DISPATCH_
#define _LIB_FUEL_CELL_DISPATCH_

#include "lib_fuel_cell.h"

class FuelCellDispatch
{
public:
	/// Default constructor
	FuelCellDispatch() { /* Nothing to do */ };

	/// Construct with arguments
	FuelCellDispatch(FuelCell * fuelCell, size_t numberOfUnits, int dispatchOption, int shutdownOption, double dt_hour);

	/// Destructor
	~FuelCellDispatch() { /* Nothing to do */ };

	/// Run dispatch for single step
	void runSingleTimeStep(double powerSystem_kWac=0, double powerLoad_kWac=0);

private:

	size_t m_numberOfUnits;
	int m_dispatchOption;
	int m_shutdownOption;
	double dt_hour;
	std::vector< FuelCell *> m_fuelCellVector;
};

#endif
