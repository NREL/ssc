#ifndef __csp_solver_mspt_collector_receiver_
#define __csp_solver_mspt_collector_receiver_

#include "csp_solver_core.h"
#include "csp_solver_pt_heliostatfield.h"
#include "csp_solver_mspt_receiver_222.h"

class C_csp_mspt_collector_receiver : public C_csp_collector_receiver
{
private:
	C_pt_heliostatfield &mc_pt_heliostatfield;
	C_mspt_receiver_222 &mc_mspt_receiver_222;

public:
	C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield, 
		C_mspt_receiver_222 & mspt_receiver_222);

	~C_csp_mspt_collector_receiver();

	virtual void init(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual int get_operating_state();

    virtual double get_startup_time();
    virtual double get_startup_energy(double step /*sec*/); //MWh
    virtual double get_pumping_parasitic_coef();  //MWe/MWt
    virtual double get_min_power_delivery();    //MWt

    virtual void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

    virtual double calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim );
  
    virtual double calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/ );

    virtual double get_collector_area();
};








#endif //__csp_solver_mspt_collector_receiver_