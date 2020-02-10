#ifndef _CMOD_CSP_COMMON_EQNS_H_
#define _CMOD_CSP_COMMON_EQNS_H_

double Csp_nameplate(double P_ref /*MWe*/, double gross_net_conversion_factor /*-*/);       // MWe

double Csp_q_pb_design(double P_ref /*MWe*/, double design_eff /*-*/);      // MWt

double Csp_Q_rec_des(double solarm /*-*/, double q_pb_design /*MWt*/);      // MWt

double Csp_tshours_sf(double tshours /*hr*/, double solarm /*-*/);          // hr

#endif