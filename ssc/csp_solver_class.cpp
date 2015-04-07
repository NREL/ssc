#include "csp_solver_core.h"

void C_csp_solver::setup_technology_model(C_csp_weatherreader *p_weatherreader, C_csp_collector_receiver *p_collector_receiver)
{
	mp_weatherreader = p_weatherreader;
	mp_collector_receiver = p_collector_receiver;

	mp_weatherreader->init();
	mp_collector_receiver->init();
}