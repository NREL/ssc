#include "core.h"

#include "lib_pvwatts.h"

static var_info _cm_vtab_pvwatts[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "dn",                         "Direct normal radiation",        "W/m2",   "",                      "Weather",      "*",                       "",                               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "df",                         "Diffuse radiation",              "W/m2",   "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                "" },
	{ SSC_INPUT,        SSC_ARRAY,       "tdry",                       "Dry bulb temp",                  "'C",     "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wspd",                       "Wind speed",                     "m/s",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                "" },

	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                       "MIN=0.5,MAX=100000",             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate value",            "frac",   "",                      "PVWatts",      "*",                       "MIN=0,MAX=1",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "PVWatts",      "*",                       "MIN=0,MAX=2",                    "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                       "MIN=0,MAX=360",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "H=0,V=90",              "PVWatts",      "*",                       "MIN=0,MAX=90",                   "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "?",                       "BOOLEAN",                        "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC array output",                "kWhdc",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                         "AC system output",               "kWhac",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                "" },

var_info_invalid };

static param_info _cm_param_tab[] = {
	/* TYPE,      NAME,       DEFAULT_VALUE,    DESCRIPTION */
	{ SSC_NUMBER, "t_start",  "0",              "Simulation start time (hours), 0 = Jan 1st, 12am" },
	{ SSC_NUMBER, "t_end",    "8759",           "Simulation end time (hours), 8759 = Dec 31st 11pm" },
	{ SSC_NUMBER, "t_step",   "1",              "Simulation time step (hours), must represent integer number of minutes" },
	{ SSC_INVALID, NULL }  };
	
class cm_pvwatts : public compute_module
{
private:
public:
	cm_pvwatts()
	{
		add_var_info( _cm_vtab_pvwatts );
		set_param_info( _cm_param_tab );
	}

	bool exec( ) throw( general_error )
	{
		float t_start = (float)param_number("t_start");
		float t_end = (float)param_number("t_end");
		float t_step = (float)param_number("t_step");

		int data_values = check_timestep( t_start, t_end, t_step );



		return false;
	}
};

DEFINE_MODULE_ENTRY( pvwatts, "Integrated PV system simulator.", 1 )
