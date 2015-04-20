#ifndef __csp_solver_mspt_collector_receiver_
#define __csp_solver_mspt_collector_receiver_

#include "csp_solver_core.h"
#include "csp_solver_pt_heliostatfield.h"
#include "csp_solver_mspt_receiver_222.h"

class C_csp_mspt_collector_receiver : public C_csp_collector_receiver
{
private:
	C_pt_heliostatfield mc_pt_heliostatfield;
	C_mspt_receiver_222 mc_mspt_receiver_222;

public:
	C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield, 
		C_mspt_receiver_222 & mspt_receiver_222);

	~C_csp_mspt_collector_receiver();

	virtual void init();

	virtual void call(const C_csp_weatherreader::S_outputs *p_weather,
		C_csp_solver_htf_state *p_htf_state,
		const C_csp_collector_receiver::S_csp_cr_inputs *p_inputs,
		const C_csp_solver_sim_info *p_sim_info);

	//virtual void converged();
};








#endif //__csp_solver_mspt_collector_receiver_