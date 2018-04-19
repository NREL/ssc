#ifndef __LIB_SHARED_INVERTER_H__
#define __LIB_SHARED_INVERTER_H__

#include "lib_sandia.h"
#include "lib_pvinv.h"

/**
*
* \class SharedInverter
*
*  A SharedInverter describes a PV inverter that can be optionally hooked up to a DC-connected battery which
*  combines with the PV output through the shared inverter.
*/
class SharedInverter
{
public:

	/// Construct a shared inverter by registering the previously constructed inverter 
	SharedInverter(int inverterType, int numberOfInverters,
		sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter);

	/// Given the combined PV plus battery DC power and voltage, compute the AC power
	void calculateACPower(const double powerDC, const double DCStringVoltage,
		double & powerAC, double & efficiencyAC, double & powerClipLoss, double & powerConsumptionLoss, double & powerNightLoss);

	enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, NONE };

protected:
	int m_inverterType;  /// The inverter type
	int m_numInverters;  /// The number of inverters in the system

						 // Memory managed elsewehre
	sandia_inverter_t * m_sandiaInverter;
	partload_inverter_t * m_partloadInverter;
};


#endif

