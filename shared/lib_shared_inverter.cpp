#include "lib_shared_inverter.h"
#include "lib_util.h"
#include <algorithm>

SharedInverter::SharedInverter(int inverterType, size_t numberOfInverters,
	sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter)
{
	m_inverterType = inverterType;
	m_numInverters = numberOfInverters;
	m_sandiaInverter = sandiaInverter;
	m_partloadInverter = partloadInverter;
	m_tempEnabled = false;
}

bool sortByVoltage(std::vector<double> i, std::vector<double> j)
{
	return (i[0] < j[0]);
}

bool SharedInverter::setTempDerateCurves(std::vector<std::vector<double>> derateCurves)
{
	// Check derate curves have V > 0, and that for each pair T > -273, slope < 0
	for (size_t r = 0; r < derateCurves.size(); r++) {
		if (derateCurves[r][0] <= 0.) return -(r+1);
		size_t tempSlopeEntries = derateCurves[r].size() - 1;
		if ((tempSlopeEntries % 2) != 0) return -(r + 1);
		for (size_t p = 0; p < tempSlopeEntries / 2; p++) {
			if ( derateCurves[r][2*p+1] <= -273. || derateCurves[r][2*p+2] >= 0.)  return -(r + 1);
		}
		m_thermalDerateCurves.push_back(derateCurves[r]);
	}

	// Sort by DC voltage
	std::sort(m_thermalDerateCurves.begin(), m_thermalDerateCurves.end(), sortByVoltage);
	
	if (m_thermalDerateCurves.size() > 0 ) m_tempEnabled = true;
	return m_tempEnabled;
}

std::vector<std::vector<double>> SharedInverter::getTempDerateCurves() {
	return m_thermalDerateCurves;
}

void SharedInverter::calculateTempDerate(double V, double T, double& pAC, double& eff, double& loss)
{
	if (eff == 0. || pAC == 0.) return;

	double slope = 0.0;
	double deltaT = 0.0;

	size_t idx = 0;
	while (V > m_thermalDerateCurves[idx][0] && idx < m_thermalDerateCurves.size()) {
		idx++;
	}

	// Find temp and slope of lower curve
	size_t p = 0;
	while (T > m_thermalDerateCurves[idx - 1][2 * p + 1] && p < m_thermalDerateCurves[idx - 1].size()) {
		p++;
	}
	deltaT = T - m_thermalDerateCurves[idx - 1][2 * (p - 1) + 1];
	slope = m_thermalDerateCurves[idx - 1][2 * (p - 1) + 1];

	// Average with values from upper curve if it exists
	if (idx < m_thermalDerateCurves.size()) {
		p = 0;
		while (T > m_thermalDerateCurves[idx][2 * p + 1] && p < m_thermalDerateCurves[idx].size()) {
			p++;
		}
		deltaT = (deltaT + (T - m_thermalDerateCurves[idx][2 * (p - 1) + 1])) / 2.;
		slope = (slope + m_thermalDerateCurves[idx][2 * (p - 1)]) / 2.;
	}
	
	// Power in units of W, eff as ratio
	double pDC = pAC/eff;
	eff += deltaT*slope;
	if (eff < 0) eff = 0.;
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
		calculateTempDerate(DCStringVoltage, T, powerAC_kW, efficiencyAC, tempLoss);
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
