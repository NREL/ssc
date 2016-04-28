#ifndef __csp_solver_trough_collector_receiver_
#define __csp_solver_trough_collector_receiver_

#include "csp_solver_core.h"

class C_csp_trough_collector_receiver : public C_csp_collector_receiver
{

public:

	// Design Parameters: Must be defined before member functions can be used
	// **************************************************************************


	int m_nSCA;		//Number of SCA's in a loop
	int m_nHCEt;		//Number of HCE types
	int m_nColt;		//Number of collector types
	int m_nHCEVar;		//Number of HCE variants per type





	// **************************************************************************
	// **************************************************************************
	// **************************************************************************

	C_csp_trough_collector_receiver();

	~C_csp_trough_collector_receiver(){};

	virtual void init();

	virtual int get_operating_state();

	virtual double get_startup_time();
	virtual double get_startup_energy(double step /*sec*/); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt

	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_state &htf_state,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

	
};







#endif