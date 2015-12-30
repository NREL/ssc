#include "csp_solver_trough_collector_receiver.h"

C_csp_trough_collector_receiver::C_csp_trough_collector_receiver()
{
	m_nSCA = -1;
	m_nHCEt = -1;
	m_nColt = -1;
	m_nHCEVar = -1;
}

void C_csp_trough_collector_receiver::init()
{
	
	double some_calc = m_nSCA + m_nHCEt;

	return;
}

int C_csp_trough_collector_receiver::get_operating_state()
{
	return -1;
}


double C_csp_trough_collector_receiver::get_startup_time()
{
	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_trough_collector_receiver::get_startup_energy(double step /*sec*/)
{
	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_trough_collector_receiver::get_pumping_parasitic_coef()
{
	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_trough_collector_receiver::get_min_power_delivery()
{
	return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_trough_collector_receiver::get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	return;
}

void C_csp_trough_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_state &htf_state,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
	const C_csp_solver_sim_info &sim_info)
{
	return;
}

void C_csp_trough_collector_receiver::converged()
{
	return;
}

double C_csp_trough_collector_receiver::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim)
{
	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_trough_collector_receiver::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/)
{
	return std::numeric_limits<double>::quiet_NaN();
}
double C_csp_trough_collector_receiver::get_collector_area()
{
	return std::numeric_limits<double>::quiet_NaN();
}






