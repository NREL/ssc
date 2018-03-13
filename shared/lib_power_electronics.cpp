#include "lib_power_electronics.h"

double bidirectional_inverter::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}
double bidirectional_inverter::convert_to_ac(double P_dc, double * P_ac)
{
	double P_loss = P_dc * (1 - _dc_ac_efficiency);
	*P_ac = P_dc * _dc_ac_efficiency;
	return P_loss;
}
double bidirectional_inverter::compute_dc_from_ac(double P_ac)
{
	return P_ac / _dc_ac_efficiency;
}

double rectifier::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}

ACBatteryController::ACBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double, double)
{
	m_batteryMetrics = battery_metrics;
	m_dispatch = dispatch;

	if (dynamic_cast<dispatch_manual_front_of_meter_t*>(m_dispatch)) {
		std::unique_ptr<dispatch_t> tmp(new dispatch_manual_front_of_meter_t(*dispatch));
		m_dispatchInitial = std::move(tmp);
	}
	if (dynamic_cast<dispatch_manual_t*>(m_dispatch)) {
		std::unique_ptr<dispatch_t> tmp2(new dispatch_manual_t(*dispatch));
		m_dispatchInitial = std::move(tmp2);
	}
	if (dynamic_cast<dispatch_automatic_behind_the_meter_t*>(m_dispatch)) {
		std::unique_ptr<dispatch_t> tmp3(new dispatch_automatic_behind_the_meter_t(*dispatch));
		m_dispatchInitial = std::move(tmp3);
	}
	if (dynamic_cast<dispatch_automatic_front_of_meter_t*>(m_dispatch)) {
		std::unique_ptr<dispatch_t> tmp4(new dispatch_automatic_front_of_meter_t(*dispatch));
		m_dispatchInitial = std::move(tmp4);
	}

	initialize(0, 0, 0, 0);
}