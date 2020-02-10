#include "cmod_csp_common_eqns.h"
#include "vartab.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'


double Csp_nameplate(double P_ref /*MWe*/, double gross_net_conversion_factor /*-*/) {      // MWe
    return P_ref * gross_net_conversion_factor;
}

double Csp_q_pb_design(double P_ref /*MWe*/, double design_eff /*-*/) {     // MWt
    return P_ref / design_eff;
}

double Csp_Q_rec_des(double solarm /*-*/, double q_pb_design /*MWt*/) {     // MWt
    return solarm * q_pb_design;
}

double Csp_tshours_sf(double tshours /*hr*/, double solarm /*-*/) {         // hr
    return tshours / solarm;
}
