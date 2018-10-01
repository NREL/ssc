#ifndef __LIB_SHARED_INVERTER_H__
#define __LIB_SHARED_INVERTER_H__

#include "lib_sandia.h"
#include "lib_pvinv.h"
#include "lib_ondinv.h"
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
	SharedInverter(int inverterType, size_t numberOfInverters,
		sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter, ond_inverter * ondInverter);

	/// Setup efficiency vs ambient T curves for temp derating, returns which curve has error if fails, 0 success
	int setTempDerateCurves(std::vector<std::vector<double>> tempDerateCurves);

	std::vector<std::vector<double>> getTempDerateCurves();

	/// Modifies pAc, eff, and loss by calculating derate, using curves interpolated by input V
	void calculateTempDerate(double V, double T, double& pAC, double& eff, double& loss);

	/// Given the combined PV plus battery DC power (W), voltage and ambient T, compute the AC power (kW)
	void calculateACPower(const double powerDC, const double DCStringVoltage, double ambientT);

	/// Return the nominal DC voltage input
	double getInverterDCNominalVoltage();

	/// Return the efficiency at max power (Paco, Vdco);
	double getMaxPowerEfficiency();

	enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, OND_INVERTER, NONE };

public:

	// calculated values for the current timestep
	double powerDC_kW;
	double powerAC_kW;
	double efficiencyAC;
	double powerClipLoss_kW;
	double powerConsumptionLoss_kW;
	double powerNightLoss_kW;
	double powerTempLoss_kW;
	double powerLossTotal_kW;
	double dcWiringLoss_ond_kW;
	double acWiringLoss_ond_kW;

protected:

	int m_inverterType;  /// The inverter type
	size_t m_numInverters;  /// The number of inverters in the system

	/// Temperate Derating: each curve contains DC voltage and pairs of start-derate temp [C] and slope [efficiency% lost per C]
	bool m_tempEnabled;
	std::vector<std::vector<double>> m_thermalDerateCurves;		/// ordered by DC V	
	/// Given a temp, find which slope to apply
	void findPointOnCurve(size_t idx, double T, double& startT, double& slope);

	// Memory managed elsewehre
	sandia_inverter_t * m_sandiaInverter;
	partload_inverter_t * m_partloadInverter;
	ond_inverter * m_ondInverter;
};


#endif
