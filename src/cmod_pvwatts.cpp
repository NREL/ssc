#include <math.h>

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

#include "core.h"

#include "lib_pvwatts.h"
#include "lib_irradproc.h"

static var_info _cm_vtab_pvwatts[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "year",                       "Year (defaults to 1990)",        "",       "",                      "Weather",      "?=1990",                  "INTEGER,MIN=1950",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lat",                        "Latitude",                       "deg",    "",                      "Weather",      "*",                       "MIN=-90,MAX=90",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lon",                        "Longitude",                      "deg",    "",                      "Weather",      "*",                       "MIN=-180,MAX=180",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tz",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "dn",                         "Direct normal radiation",        "W/m2",   "",                      "Weather",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "df",                         "Diffuse radiation",              "W/m2",   "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "tdry",                       "Dry bulb temp",                  "'C",     "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wspd",                       "Wind speed",                     "m/s",    "",                      "Weather",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                       "MIN=0.5,MAX=100000",                       "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate value",            "frac",   "",                      "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "PVWatts",      "*",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "PVWatts",      "*",                       "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "?",                       "BOOLEAN",                                  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "t_noct",                     "Nominal operating cell temperature", "'C", "",                      "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_ref",                      "Reference cell temperature",     "'C",     "",                      "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma",                      "Max power temperature coefficient", "%/'C", "",                     "PVWatts",      "?=-0.5",                  "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                    "Inverter efficiency at rated power", "frac", "",                    "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                     "Ground reflectivity",            "frac",   "",                      "PVwatts",      "?=0.2",                   "MIN=0,MAX=1",                              "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "time",                       "Time",                           "hours",  "0=Jan1st,12am",         "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "month",                      "Month",                          "",       "1-12",                  "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "day",                        "Day",                            "",       "1-365",                 "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hour",                       "Hour",                           "",       "0-23",                  "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "minute",                     "Minute",                         "",       "0-59",                  "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_azm",                    "Solar azimuth",                  "deg",    "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_zen",                    "Solar zenith",                   "deg",    "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_elv",                    "Sun elevation",                  "deg",    "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_dec",                    "Sun declination",                "deg",    "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_beam",                   "Incident beam irradiance (POA)", "W/m2",   "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_diffuse",                "Incident diffuse irradiance (POA)", "W/m2","",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "t_cell",                     "Cell temperature",               "'C",     "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC array output",                "kWhdc",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                         "AC system output",               "kWhac",  "",                      "PVWatts",      "*",                       "LENGTH_EQUAL=dn",                          "" },

var_info_invalid };

static param_info _cm_param_tab[] = {
	/* TYPE,      NAME,       DEFAULT_VALUE,    DESCRIPTION */
	{ SSC_NUMBER, "t_start",  "0",              "Simulation start time (sec), 0 = Jan 1st, 12am" },
	{ SSC_NUMBER, "t_end",    "31536000",       "Simulation end time (sec), 8759*3600 = Dec 31st 11pm" },
	{ SSC_NUMBER, "t_step",   "3600",           "Simulation time step (sec)" },
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

	void exec( ) throw( general_error )
	{
		double t_start = (double)param_number("t_start");
		double t_end = (double)param_number("t_end");
		double t_step = (double)param_number("t_step");

		size_t num_steps = check_timestep_seconds( t_start, t_end, t_step );

		size_t arr_len;
		ssc_number_t *p_dn = as_array( "dn", &arr_len );      if (arr_len != num_steps) throw mismatch_error( (int)num_steps, (int)arr_len, "direct normal radiation");
 		ssc_number_t *p_df = as_array( "df", &arr_len );      if (arr_len != num_steps) throw mismatch_error( (int)num_steps, (int)arr_len, "diffuse radiation");
		ssc_number_t *p_tdry = as_array( "tdry", &arr_len );  if (arr_len != num_steps) throw mismatch_error( (int)num_steps, (int)arr_len, "dry bulb temperature");
		ssc_number_t *p_wspd = as_array( "wspd", &arr_len );  if (arr_len != num_steps) throw mismatch_error( (int)num_steps, (int)arr_len, "wind speed");
		
		int year = as_integer("year");
		double lat = as_double("lat");
		double lon = as_double("lon");
		double tz = as_double("tz");

		double watt_spec = 1000.0 * as_double("system_size");
		double derate = as_double("derate");
		int track_mode = as_integer("track_mode"); // 0, 1, 2
		double tilt = as_double("tilt");
		double azimuth = as_double("azimuth");
		if ( lookup("tilt_eq_lat") && as_boolean("tilt_eq_lat") ) tilt = lat; // override tilt angle

		ssc_number_t *p_poa_beam = allocate("poa_beam", num_steps);
		ssc_number_t *p_poa_diffuse = allocate("poa_diffuse", num_steps);
		ssc_number_t *p_tcell = allocate("tcell", num_steps);
		ssc_number_t *p_dc = allocate("dc", num_steps);
		ssc_number_t *p_ac = allocate("ac", num_steps);

		ssc_number_t *p_time = allocate("time", num_steps);
		ssc_number_t *p_month = allocate("month", num_steps);
		ssc_number_t *p_day = allocate("day", num_steps);
		ssc_number_t *p_hour = allocate("hour", num_steps);
		ssc_number_t *p_minute = allocate("minute", num_steps);
		ssc_number_t *p_azm = allocate("sun_azm", num_steps);
		ssc_number_t *p_zen = allocate("sun_zen", num_steps);
		ssc_number_t *p_elv = allocate("sun_elv", num_steps);
		ssc_number_t *p_dec = allocate("sun_dec", num_steps);

		
		/* PV RELATED SPECIFICATIONS */

		// note: these are normally hard-wired PVwatts constants, but made 
		// available for this most "advanced" of pvwatts implementations...  (5 aug 2011, apd)

		double inoct = as_double("t_noct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double reftem = as_double("t_ref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */
		double albedo = as_double("albedo"); // PVWATTS_ALBEDO;                 /* surface albedo, decimal fraction */

		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double rot_limit = PVWATTS_ROTLIM;             /* +/- rotation in degrees permitted by physical constraint of tracker */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */

		/* storage for calculations */
		double angle[3];
		double sun[8];

		double Ts = t_start;
		size_t idx = 0;
		
		while ( Ts < t_end && idx < num_steps )
		{
			// calculate month, day, hour, minute time
			double time = Ts/3600;

			int month = util::month_of(time) ;              // month goes 1-12
			int day = util::day_of_month(month,time) ;   // day goes 1-nday_in_month
			int hour = ((int)time)%24;		         // hour goes 0-23
			int minute = (int)( (time-floor(time))*60  + t_step/60/2);      // minute goes 0-59
			
			// calculate solar position
			solarpos( year, month, day, hour, minute, lat, lon, tz, sun );

			if (idx % (num_steps/25)==0)
				update( "calculating", 100*((float)idx+1)/((float)num_steps), (float)time );

			double poa[3], pvt, dc, ac;
			poa[0]=poa[1]=poa[2] = 0.0;

			if (sun[2] > 0.0087)
			{
				/* sun elevation > 0.5 degrees */
				double dn = p_dn[idx];
				double df = p_df[idx];
				double wind = p_wspd[idx];
				double ambt = p_tdry[idx];

				incidence( track_mode, tilt, azimuth, rot_limit, sun[1], sun[0], angle );
				perez( sun[8], dn, df, albedo, angle[0], angle[1], sun[1], poa );

				double tpoa = 0;
				if (dn > 0)	
					tpoa = transpoa( poa[0]+poa[1]+poa[2], dn, angle[0] );  /* have valid poa and dn, calculate transmitted through glass cover */
				else
					tpoa = poa[0]+poa[1]+poa[2]; /* default to dn 0 or bad value - assume no glass cover on module */
				
				pvt = celltemp(inoct, height, poa[0]+poa[1]+poa[2], wind, ambt );
				dc = dcpowr( reftem, watt_spec, pwrdgr, tmloss, tpoa, pvt );
				ac = dctoac( watt_spec, efffp, dc );
			}
			else
			{
				/* night time */
				pvt = 999.9;
				dc = 0.0;
				ac = 0.0;
			}

			p_time[idx] = (ssc_number_t)time;
			p_month[idx] = (ssc_number_t)month;
			p_day[idx] = (ssc_number_t)day;
			p_hour[idx] = (ssc_number_t)hour;
			p_minute[idx] = (ssc_number_t)minute;
			
			p_azm[idx] = (ssc_number_t) (sun[0] * 180/M_PI);
			p_zen[idx] = (ssc_number_t) (sun[1] * 180/M_PI);
			p_elv[idx] = (ssc_number_t) (sun[2] * 180/M_PI);
			p_dec[idx] = (ssc_number_t) (sun[3] * 180/M_PI);

			p_poa_beam[idx] = (ssc_number_t)poa[0];
			p_poa_diffuse[idx] = (ssc_number_t)(poa[1]+poa[2]);
			p_tcell[idx] = (ssc_number_t)pvt;
			p_dc[idx] = (ssc_number_t)dc;
			p_ac[idx] = (ssc_number_t)ac;

			Ts += t_step;
			idx++;
		}
	}
};

DEFINE_MODULE_ENTRY( pvwatts, "Integrated PV system simulator.", 5 )
