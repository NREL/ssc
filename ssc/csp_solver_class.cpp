#include "csp_solver_core.h"

void C_csp_solver::setup_technology_model(C_csp_weatherreader *p_weatherreader, C_csp_collector_receiver *p_collector_receiver,
			C_csp_thermal_storage *p_thermal_storage, C_csp_power_cycle *p_power_cycle)
{
	mp_weatherreader = p_weatherreader;
	mp_collector_receiver = p_collector_receiver;
	mp_thermal_storage = p_thermal_storage;
	mp_power_cycle = p_power_cycle;

	mp_weatherreader->init();
	mp_collector_receiver->init();
	mp_thermal_storage->init();
	mp_power_cycle->init();
}

C_csp_solver::C_csp_solver()
{
	mp_weatherreader = NULL;
	mp_collector_receiver = NULL;
	mp_thermal_storage = NULL;
	mp_power_cycle = NULL;
}

void C_csp_solver::timeseries_simulation()
{
	if( mp_weatherreader == NULL || mp_collector_receiver == NULL || mp_thermal_storage == NULL || mp_power_cycle == NULL )
	{
		throw exec_error("Timeseries simulation pre-simulation checks", 
			"All pointers-to-component classes must be allocated with 'setup_technology_model' function before timeseries simulation function is called");
	}	
}