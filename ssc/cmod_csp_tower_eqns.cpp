#include "cmod_csp_tower_eqns.h"
#include "cmod_csp_common_eqns.h"
#include "vartab.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void MSPT_System_Design_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double P_ref, gross_net_conversion_factor, nameplate, design_eff, solarm, q_pb_design, Q_rec_des, tshours, tshours_sf;

    // nameplate
    vt_get_number(vt, "P_ref", &P_ref);
    vt_get_number(vt, "gross_net_conversion_factor", &gross_net_conversion_factor);
    nameplate = Csp_nameplate(P_ref, gross_net_conversion_factor);
    vt->assign("nameplate", nameplate);

    // q_pb_design
    vt_get_number(vt, "P_ref", &P_ref);                         // repeat assessors because the values may have changed
    vt_get_number(vt, "design_eff", &design_eff);
    q_pb_design = Csp_q_pb_design(P_ref, design_eff);
    vt->assign("q_pb_design", q_pb_design);

    // Q_rec_des
    vt_get_number(vt, "solarm", &solarm);
    vt_get_number(vt, "q_pb_design", &q_pb_design);
    Q_rec_des = Csp_Q_rec_des(solarm, q_pb_design);
    vt->assign("q_rec_des", Q_rec_des);

    // tshours_sf
    vt_get_number(vt, "tshours", &tshours);
    vt_get_number(vt, "solarm", &solarm);
    tshours_sf = Csp_tshours_sf(tshours, solarm);
    vt->assign("tshours_sf", tshours_sf);
}