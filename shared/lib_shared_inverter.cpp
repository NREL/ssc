#include "lib_shared_inverter.h"

SharedInverter::SharedInverter(int inverterType, int numberOfInverters,
	sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter)
{
	m_inverterType = inverterType;
	m_numInverters = numberOfInverters;
	m_sandiaInverter = sandiaInverter;
	m_partloadInverter = partloadInverter;
}

void SharedInverter::calculateACPower(const double powerDC, const double DCStringVoltage,
	double & powerAC, double & efficiencyAC, double & powerClipLoss, double & powerConsumptionLoss, double & powerNightLoss)
{
	double P_par, P_lr;
	if (m_inverterType == SANDIA_INVERTER)
		m_sandiaInverter->acpower(powerDC / m_numInverters, DCStringVoltage, &powerAC, &P_par, &P_lr, &efficiencyAC, &powerClipLoss, &powerConsumptionLoss, &powerNightLoss);
	else if (m_inverterType == PARTLOAD_INVERTER)
		m_partloadInverter->acpower(powerDC / m_numInverters, &powerAC, &P_lr, &P_par, &efficiencyAC, &powerClipLoss, &powerNightLoss);

	powerAC *= m_numInverters;
	powerClipLoss *= m_numInverters;
	powerConsumptionLoss *= m_numInverters;
	powerNightLoss *= m_numInverters;
	efficiencyAC *= 100;
}

double SharedInverter::getInverterDCNominalVoltage()
{
	if (m_inverterType == SANDIA_INVERTER)
		return m_sandiaInverter->Vdco;
	else if (m_inverterType == PARTLOAD_INVERTER)
		return m_partloadInverter->Vdco;
}
