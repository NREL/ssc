#include "core.h"

static var_info _cm_vtab_timeseq[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "start_time",                 "Start time",                     "hours", "0=jan1st 12am",         "Time Sequence", "*",                       "MIN=0,MAX=8760",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "end_time",                   "End time",                       "hours", "0=jan1st 12am",         "Time Sequence", "*",                       "MIN=0,MAX=8760",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "time_step_min",              "Time step",                      "minutes","",                     "Time Sequence", "*",                       "MIN=1,MAX=60,TS_M",                      "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "time",                       "Time",                           "hours",  "0=jan1st 12am",        "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "month",                      "Month",                          "",       "1-12",                 "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "day",                        "Day",                            "",       "1-{28,30,31}",         "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hour",                       "Hour",                           "",       "0-23",                 "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "minute",                     "Minute",                         "",       "0-59",                 "Time",          "*",                       "",                                       "" },

var_info_invalid };

class cm_timeseq : public compute_module
{
private:
public:
	cm_timeseq()
	{
		add_var_info( _cm_vtab_timeseq );
	}

	bool exec( ) throw( general_error )
	{
		double t_start = as_double("start_time");
		double t_end = as_double("end_time");
		double t_step = as_double("time_step_min")/60.0f;

		size_t num_steps = check_timestep( t_start, t_end, t_step );

		ssc_number_t *time = allocate("time", num_steps);
		ssc_number_t *month = allocate("month", num_steps);
		ssc_number_t *day = allocate("day", num_steps);
		ssc_number_t *hour = allocate("hour", num_steps);
		ssc_number_t *minute = allocate("minute", num_steps);

		double T = t_start;
		size_t idx = 0;
		while (T < t_end && idx < num_steps)
		{
			time[idx] = (float) T;
			
			int m = util::month_of(T);
			month[idx] = (ssc_number_t) m ;              // month goes 1-12
			day[idx] = (ssc_number_t) util::day_of_month(m,T) ;   // day goes 1-nday_in_month
			hour[idx] = (ssc_number_t) ((int)(T)%24);		         // hour goes 0-23
			minute[idx] = (ssc_number_t) ((int)( (T-floor(T))*60  + t_step*30.0f));      // minute goes 0-59
	
			T += t_step;
			idx++;
		}

		return true;
	}
};

DEFINE_MODULE_ENTRY( timeseq, "Hourly time sequence generator", 1 )
