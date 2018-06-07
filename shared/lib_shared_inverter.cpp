#include "lib_shared_inverter.h"
#include "lib_util.h"

SharedInverter::SharedInverter(int inverterType, int numberOfInverters,
	sandia_inverter_t * sandiaInverter, partload_inverter_t * partloadInverter)
{
	m_inverterType = inverterType;
	m_numInverters = numberOfInverters;
	m_sandiaInverter = sandiaInverter;
	m_partloadInverter = partloadInverter;
}

void SharedInverter::calculateACPower(const double powerDC_Watts, const double DCStringVoltage)
{
	double P_par, P_lr;

	// Power quantities go in and come out in units of W
	if (m_inverterType == SANDIA_INVERTER || m_inverterType == DATASHEET_INVERTER || m_inverterType == COEFFICIENT_GENERATOR)
		m_sandiaInverter->acpower(powerDC_Watts / m_numInverters, DCStringVoltage, &powerAC_kW, &P_par, &P_lr, &efficiencyAC, &powerClipLoss_kW, &powerConsumptionLoss_kW, &powerNightLoss_kW);
	else if (m_inverterType == PARTLOAD_INVERTER)
		m_partloadInverter->acpower(powerDC_Watts / m_numInverters, &powerAC_kW, &P_lr, &P_par, &efficiencyAC, &powerClipLoss_kW, &powerNightLoss_kW);

	// Convert units to kW
	powerDC_kW = powerDC_Watts * util::watt_to_kilowatt;
	powerAC_kW *= m_numInverters * util::watt_to_kilowatt;
	powerClipLoss_kW *= m_numInverters * util::watt_to_kilowatt;
	powerConsumptionLoss_kW *= m_numInverters * util::watt_to_kilowatt;
	powerNightLoss_kW *= m_numInverters * util::watt_to_kilowatt;
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
