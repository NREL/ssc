#ifndef __LIB_FUEL_CELL__
#define __LIB_FUEL_CELL__

#include "lib_util.h"

/**
* \class FuelCell
*
* \brief
*
*  The FuelCell class provides the underlying data and methods required to model fuel cell technology in SAM.
*/
class FuelCell
{
public:
	/// Default FuelCell constructor
	FuelCell();

	/// Construct FuelCell with arguments
	FuelCell(size_t numberOfUnits, double unitPowerMax_kW, double unitPowerMin_kW, double startup_hours, double dynamicResponse_kWperMin,
		double degradation_kWperHour, double degradationRestart_kW, double replacement_percent, util::matrix_t<double> efficiencyTable,
		double lowerHeatingValue_MJperkg, double availableFuel_kg,
		int shutdownOption, int dispatchOption);

	/// Default destructor
	~FuelCell();

protected:

	enum FC_SHUTDOWN_OPTION { SHUTDOWN, IDLE};
	enum FC_DISPATCH_OPTION { FIXED, LOAD_FOLLOW, MANUAL};

	size_t numberOfUnits;
	double unitPowerMax_kW;
	double unitPowerMin_kW;
	double startup_hours;
	double dynamicResponse_kWperMin;
	double degradation_kWperHour;
	double degradationRestart_kW;
	double replacement_percent;
	util::matrix_t<double> efficiencyTable;
	double lowerHeatingValue_MJperKg;
	double availableFuel_kg;
	int shutdownOption;
	int dispatchOption;

};

#endif __LIB_FUEL_CELL__
