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
		double lowerHeatingValue_MJperkg, double higherHeatingValue_MJperkg, double availableFuel_MCf,
		int shutdownOption, int dispatchOption, double dt_hour);

	/// Default destructor
	~FuelCell();

	/// Run for single time step
	void runSingleTimeStep() {};

	/// Option enumerations
	enum FC_SHUTDOWN_OPTION { SHUTDOWN, IDLE };
	enum FC_DISPATCH_OPTION { FIXED, LOAD_FOLLOW, MANUAL };

protected:

	enum FC_EFFICIENCY_COLUMN { PERCENT_MAX, PRECENT_ELECTRICAL_EFFICIENCY, PERCENT_HEAT_RECOVERY };

	// input values
	double dt_hour;
	size_t m_numberOfUnits;
	double m_unitPowerMax_kW;
	double m_unitPowerMin_kW;
	double m_startup_hours;
	double m_dynamicResponse_kWperMin;
	double m_degradation_kWperHour;
	double m_degradationRestart_kW;
	double m_replacement_percent;
	util::matrix_t<double> m_efficiencyTable;
	double m_lowerHeatingValue_BtuPerFt3;
	double m_higherHeatingValue_BtuPerFt3;
	double m_availableFuel_MCf;
	int m_shutdownOption;
	int m_dispatchOption;

	// calculated
	bool m_startedUp;
	double m_hoursSinceStart;
	std::vector<double> m_fuelConsumption;
	double m_availableFuel_kWh;
};

#endif __LIB_FUEL_CELL__
