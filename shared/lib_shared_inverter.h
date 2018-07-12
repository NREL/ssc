#ifndef __LIB_SHARED_INVERTER_H__
#define __LIB_SHARED_INVERTER_H__

#include "lib_sandia.h"
#include "lib_pvinv.h"
#include <vector>

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

	/// Setup efficiency vs ambient T curves at up to 3 input voltages for temp derating, returns true if successful
	bool setTempDerateCurves(double* curve1, double* curve2 = NULL, double* curve3 = NULL );

	void getTempDerateCurves(double* vParts, double* startC, double* slope);

	/// Given DC voltage and ambient temperate, calculate derated power, eff and loss
	void calculateTempDerate(double V, double T, double& pAC, double& eff, double& loss);

	/// Given the combined PV plus battery DC power (W), voltage and ambient T, compute the AC power (kW)
	void calculateACPower(const double powerDC, const double DCStringVoltage, double ambientT = 0.0);

	/// Return the nominal DC voltage input
	double getInverterDCNominalVoltage();

	enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, NONE };

public:

	// calculated values for the current timestep
	double powerDC_kW;
	double powerAC_kW;
	double efficiencyAC;
	double powerClipLoss_kW;
	double powerConsumptionLoss_kW;
	double powerNightLoss_kW;
	double powerTempLoss_kW;

protected:

	int m_inverterType;  /// The inverter type
	int m_numInverters;  /// The number of inverters in the system

	// temperature derate curves
	bool m_tempEnabled;
	double m_tempV[2] = { 0, 0 };					/// ordered DC voltages which divide operating V range into up to 2 partitions
	double m_tempStartC[3] = { -99, -99, -99 }; 	/// for each V range, the temperature at which derate begins to be applied
	double m_tempSlope[3] = { 0, 0, 0 };			/// for each V range, the slope, efficiency%/degree C

	// Memory managed elsewehre
	sandia_inverter_t * m_sandiaInverter;
	partload_inverter_t * m_partloadInverter;
};


#endif
