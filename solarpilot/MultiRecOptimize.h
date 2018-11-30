#ifndef _MULTI_REC_
#define _MULTI_REC_

#define LPWINAPP
#include <vector>
#include <string>
#include <sstream>

class SolarField;
class simulation_info;

struct multi_rec_opt_helper
{
    enum RESULT_STATUS { RS_NOSIM=-1, RS_OPTIMAL=0, RS_SUBOPTIMAL, RS_INFEASIBLE, RS_TIMED_OUT, RS_UNKNOWN_ERROR};

    bool is_abort_flag;
    bool is_performance;
    int result_status;
    double objective;
    Hvector included_heliostats;
    double timeout_sec;
    simulation_info *sim_info;
    int solver_status;
    double last_report_time;
    double sim_report_step;
    std::string problem_name;

    multi_rec_opt_helper()
    {
        is_performance = false;
        is_abort_flag = false;
        result_status = RESULT_STATUS::RS_NOSIM;
        timeout_sec = 10000;
        objective = 0.;
        sim_info = 0;
        solver_status = std::numeric_limits<int>::quiet_NaN();
        last_report_time = -1.;
        sim_report_step = 1.;
        problem_name = "Optimization problem";
    };

    int run(SolarField *SF);
};



#endif
