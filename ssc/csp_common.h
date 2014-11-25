#ifndef _CSP_COMMON_
#define _CSP_COMMON_ 1

#include "core.h"
#include "AutoPilot_API.h"

class solarpilot_invoke
{
    compute_module *m_cmod;
    AutoPilot_S *m_sapi;


public:
    sp_optimize opt;
    sp_ambient amb;
    sp_cost cost;
    sp_heliostats helios;
    sp_receivers recs;
    sp_layout layout;
    sp_flux_table fluxtab;

    solarpilot_invoke( compute_module *cm );
    ~solarpilot_invoke();
    AutoPilot_S *GetSAPI();
    bool run();
};


#endif