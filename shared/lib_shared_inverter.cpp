#include "lib_shared_inverter.h"
#include "lib_util.h"
#include <algorithm>

SharedInverter::SharedInverter(int inverterType, int numberOfInverters,
	sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter)
{
	m_inverterType = inverterType;
	m_numInverters = numberOfInverters;
	m_sandiaInverter = sandiaInverter;
	m_partloadInverter = partloadInverter;
	m_tempEnabled = false;
	m_tempV = {0, 0};
	m_tempSlope = {0, 0, 0};
	m_tempStartC = {-99, -99, -99};
}

bool sortByVoltage(double[] i, double[] j)
{
	return (i[0] < j[0]);
}

bool setTempDerateCurves(double[] curve1, double[] curve2, double[] curve3 )
{
	if (curve1[0] < 0 || curve1[1] < 0 || curve1[2] >= 0) return false;

	// if multiple curves provided, partition operating V range
	std::vector<double*> derateCurves;
	if (curve2[0] > 0){
		if (curve2[1] <= -99 || curve2[2] >= 0) return false;
		derateCurves.push_back(curve2);
	}
	if (curve3[0] > 0){
		if (curve3[1] <= -99 || curve3[2] >= 0) return false;
		derateCurves.push_back(curve3);
	}

	std::sort(derateCurves.begin(), derateCurves.end(), sortByVoltage);
	m_tempStartC[0] = derateCurves[0][1];
	m_tempSlope[0] = derateCurves[0][2];

	if (derateCurves.size() > 1){
		m_tempV[0] = (derateCurves[1][0] - derateCurves[0][0]/2);
		m_tempStartC[1] = derateCurves[1][1];
		m_tempSlope[1] = derateCurves[1][2];
		if (derateCurves.size() > 2) {
			m_tempV[1] = (derateCurves[2][0] - derateCurves[1][0]/2);
			m_tempStartC[3] = derateCurves[3][1];
			m_tempSlope[3] = derateCurves[3][2];
		}
	}

	m_tempEnabled = true;
	return true;
}

void SharedInverter::calculateTempDerate(double V, double T, double& pAC, double& eff, double& loss)
{
	double slope = 0.0;
	double deltaT = 0.0;

	if (m_tempV[0] == 0 || (m_tempV[0] != 0 && V <= m_tempV[0])){
		if (T <= m_tempStartC[0]) return;
		slope = m_tempSlope[0];
		deltaT = T - m_tempStartC[0];
	}
	else{
		if (m_tempV[1] == 0 || (m_tempV[1] != 0 && V <= m_tempV[1])){
			if (T <= m_tempStartC[1]) return;
			slope = m_tempSlope[1];
			deltaT = T - m_tempStartC[1];
		}
		else{
			if (T <= m_tempStartC[2]) return;
			slope = m_tempSlope[2];
			deltaT = T - m_tempStartC[2];
		}
	}

	double pDC = pAC/eff;
	eff -= deltaT*slope;
	loss = pAC - (pDC * eff);
	pAC = pDC * eff;	
}

void SharedInverter::calculateACPower(const double powerDC_Watts, const double DCStringVoltage, double T)
{
	double P_par, P_lr;

	// Power quantities go in and come out in units of W
	if (m_inverterType == SANDIA_INVERTER || m_inverterType == DATASHEET_INVERTER || m_inverterType == COEFFICIENT_GENERATOR)
		m_sandiaInverter->acpower(powerDC_Watts / m_numInverters, DCStringVoltage, &powerAC_kW, &P_par, &P_lr, &efficiencyAC, &powerClipLoss_kW, &powerConsumptionLoss_kW, &powerNightLoss_kW);
	else if (m_inverterType == PARTLOAD_INVERTER)
		m_partloadInverter->acpower(powerDC_Watts / m_numInverters, &powerAC_kW, &P_lr, &P_par, &efficiencyAC, &powerClipLoss_kW, &powerNightLoss_kW);

	double tempLoss = 0.0;
	if (m_tempEnabled){
		calculateTempDerate(DCStringVoltage, T, &powerAC_kW, &efficiencyAC, &tempLoss);
	}

	// Convert units to kW
	powerDC_kW = powerDC_Watts * util::watt_to_kilowatt;
	powerAC_kW *= m_numInverters * util::watt_to_kilowatt;
	powerClipLoss_kW *= m_numInverters * util::watt_to_kilowatt;
	powerConsumptionLoss_kW *= m_numInverters * util::watt_to_kilowatt;
	powerNightLoss_kW *= m_numInverters * util::watt_to_kilowatt;
	powerTempLoss_kW = tempLoss * m_numInverters * util::watt_to_kilowatt;
	efficiencyAC *= 100;
}

double SharedInverter::getInverterDCNominalVoltage()
{
	if (m_inverterType == SANDIA_INVERTER)
		return m_sandiaInverter->Vdco;
	else if (m_inverterType == PARTLOAD_INVERTER)
		return m_partloadInverter->Vdco;
	else
		return 0.;
}
