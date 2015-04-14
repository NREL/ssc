#include "csp_solver_mspt_collector_receiver.h"

C_csp_mspt_collector_receiver::C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield,
	C_mspt_receiver_222 & mspt_receiver_222)
{
	mc_pt_heliostatfield = pt_heliostatfield;
	mc_mspt_receiver_222 = mspt_receiver_222;
}

C_csp_mspt_collector_receiver::~C_csp_mspt_collector_receiver()
{}

void C_csp_mspt_collector_receiver::init()
{
	mc_pt_heliostatfield.init();
	mc_mspt_receiver_222.init();
}