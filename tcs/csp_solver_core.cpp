#include "csp_solver_core.h"
#include "csp_solver_util.h"

C_csp_solver::C_csp_solver(C_csp_weatherreader *p_weather,
	C_csp_collector_receiver *p_collector_receiver,
	C_csp_power_cycle *p_power_cycle)
{
	mpc_weather = p_weather;
	mpc_collector_receiver = p_collector_receiver;
	mpc_power_cycle = p_power_cycle;

	init_independent();
}

void C_csp_solver::init_independent()
{
	mpc_weather->init();
	mpc_collector_receiver->init();
	mpc_power_cycle->init();

	return;
}