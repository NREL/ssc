#ifndef _MULTI_REC_
#define _MULTI_REC_

#define LPWINAPP
#include <vector>
#include <string>
#include <sstream>

//#include "definitions.h"

class SolarField;

struct multi_rec_opt_helper
{
    enum RESULT_STATUS { RS_NOSIM=-1, RS_OPTIMAL=0, RS_SUBOPTIMAL, RS_INFEASIBLE, RS_TIMED_OUT, RS_UNKNOWN_ERROR};

    bool is_abort_flag;
    bool is_performance;
    int result_status;
    std::stringstream simlog;
    double objective;
    Hvector included_heliostats;
    double timeout_sec;

    multi_rec_opt_helper()
    {
        is_performance = false;
        is_abort_flag = false;
        result_status = RESULT_STATUS::RS_NOSIM;
        simlog.clear();
        timeout_sec = 10000;
        objective = 0.;
    };

    int run(SolarField *SF);
};



#endif
