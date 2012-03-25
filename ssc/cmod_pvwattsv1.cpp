#include "core.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"


#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_pvwattsv1[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",                  "local weather file path",        "",       "",                      "Weather",      "*",                       "LOCAL_FILE",      "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                            "MIN=0.05,MAX=500000",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate value",            "frac",   "",                      "PVWatts",      "*",                            "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "PVWatts",      "*",                            "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                            "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "PVWatts",      "naof:tilt_eq_lat",             "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "na:tilt",                      "BOOLEAN",                                  "" },

	/* advanced inputs */
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                     "Tracker rotation limit (+/- 1 axis)","deg",    "",                 "PVWatts",      "?=45.0",                  "MIN=1,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_noct",                     "Nominal operating cell temperature", "'C",     "",                 "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_ref",                      "Reference cell temperature",         "'C",     "",                 "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma",                      "Max power temperature coefficient",  "%/'C",   "",                 "PVWatts",      "?=-0.5",                  "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                    "Inverter efficiency at rated power", "frac",   "",                 "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fd",                         "Diffuse fraction",                   "0..1",   "",                 "PVWatts",      "?=1.0",                   "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "i_ref",                      "Rating condition irradiance",        "W/m2",   "",                 "PVWatts",      "?=1000",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "poa_cutin",                  "Min reqd irradiance for operation",  "W/m2",   "",                 "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "w_stow",                     "Wind stow speed",                    "m/s",    "",                 "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                        "Plane of array irradiance",      "W/m2",   "",                      "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                      "Module temperature",             "'C",     "",                      "PVWatts",      "*",                       "LENGTH=8760",                          "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC array output",                "Wdc",  "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                         "AC system output",               "Wac",  "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

var_info_invalid };

class cm_pvwattsv1 : public compute_module
{
public:
	
	cm_pvwattsv1()
	{
		add_var_info( _cm_vtab_pvwattsv1 );
	}

	void exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");

		weatherfile wf( file );
		if (!wf.ok()) throw exec_error("pvwattsv1", "failed to read local weather file: " + std::string(file));
					
		double watt_spec = 1000.0 * as_double("system_size");
		double derate = as_double("derate");
		int track_mode = as_integer("track_mode"); // 0, 1, 2
		double azimuth = as_double("azimuth");
		double tilt = wf.lat;
		if ( !lookup("tilt_eq_lat") || !as_boolean("tilt_eq_lat") )
			tilt = as_double("tilt");

		ssc_number_t *p_dc = allocate("dc", 8760);
		ssc_number_t *p_ac = allocate("ac", 8760);
		ssc_number_t *p_tcell = allocate("tcell", 8760);
		ssc_number_t *p_poa = allocate("poa", 8760);
	
		/* PV RELATED SPECIFICATIONS */
		
		double inoct = as_double("t_noct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double reftem = as_double("t_ref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */
		double rlim = as_double("rotlim");             /* +/- rotation in degrees permitted by physical constraint of tracker */
		double fd = as_double("fd"); // diffuse fraction
		double i_ref = as_double("i_ref"); // reference irradiance for rating condition
		double poa_cutin = as_double("poa_cutin"); // minimum POA irradiance level required for any operation
		double wind_stow = as_double("w_stow"); // maximum wind speed before stowing.  stowing causes all output to be lost
	
		/* storage for calculations */
		double angle[5];
		double sun[9];

		double sunrise, sunset;
		int jday = 0;
		int cur_hour = 0;

		double lat = wf.lat;
		double lng = wf.lon;
		double tz = wf.tz;

		int beghr, endhr;

		double dn[24],df[24],wind[24],ambt[24],snow[24]/*,albwf[24]*/,dc[24],ac[24],poa[24],tpoa[24];
		int sunup[24];
		
		// create cell temp object at beginning (tracks hour-to-hour cell temp)

		for(int m=0;m<12;m++)   /* Loop thru a year of data a month at a time */
		{
			for(int n=1;n<=util::nday[m];n++)    /* For each day of month */
			{
				int yr=1990,mn=1,dy=1;
				jday++;                 /* Increment julian day */
				for(int i=0;i<24;i++)      /* Read a day of data and initialize */
				{
					if (!wf.read())
						throw exec_error("pvwattsv1", "could not read data line " + util::to_string(i+1) + " of 8760");
					
					dn[i] = wf.dn;
					df[i] = wf.df;
					wind[i] = wf.wspd;
					ambt[i] = wf.tdry;
					snow[i] = wf.snow;
					//albwf[i] = wf.albedo;
					poa[i]=0.0;             /* Plane-of-array radiation */
					tpoa[i]=0.0;            /* Transmitted radiation */
					dc[i]=0.0;              /* DC power */
					ac[i]=0.0;              /* AC power */
					sunup[i] = 0;

					yr = wf.year;
					mn = wf.month;
					dy = wf.day;
				}
			
				solarpos(yr,mn,dy,12,0.0,lat,lng,tz,sun);/* Find sunrise and sunset */

				sunrise = sun[4];       /* In local standard times */
				sunset = sun[5];        /* not corrected for refraction */
			
				beghr = (int)sunrise + 24;    /* Add an offset since sunrise can be negative */
				endhr = (int)(sunset - 0.01) + 24; /* Add an offset to make it track with sunrise */
			
				if( sunset - sunrise > 0.01 )
				{
					for(int i=beghr;i<=endhr;i++ )
						sunup[i%24] = 1;           /* Assign value of 1 for daytime hrs */
				}

				beghr %= 24;                     /* Remove offsets */
				endhr %= 24;

				if( sunrise < 0.0 )
					sunrise += 24.0;    /* Translate to clock hours */
				if( sunset > 24.0 )
					sunset -= 24.0;

				for(int i=0;i<24;i++ )
				{
					if( sunup[i] )
					{
						double minute = 30.0;    /* Assume the midpoint of the hour,except... */
						if( beghr != endhr )
						{
							if( i == beghr )     /* Find effective time during hr */
								minute = 60.0*( 1.0 - 0.5*( (double)i + 1.0 - sunrise ) );
							else if( i == endhr )
								minute = 60.0*0.5*( sunset - (double)i );
						
							solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
						}
						else if( i == beghr && fabs(sunset-sunrise) > 0.01 ) /* Same sunup/sunset hour */
						{  
							/* Fudge the azimuth and zenith as the mean of sunup periods */
							if( sunset > sunrise )  /* Rises and sets in same winter hour */
							{                    /* For zenith at mid-height */
								minute = 60.0*( sunrise + 0.25*( sunset - sunrise ) - (double)i );
								solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
								double tmp = sun[1];        /* Save zenith */
															/* For azimuth at midpoint */
								minute = 60.0*( sunrise + 0.5*( sunset - sunrise ) - (double)i );
								solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
								sun[1] = tmp;
							
							}
							else     /* Sets and rises in same summer hour */
							{
								double tmp = 0.0;
								double tmp2 = 0.0;
								minute = 60.0*( 1.0 - 0.5*( (double)i + 1.0 - sunrise ) );
								solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
								tmp += sun[1];
								if( sun[0]/DTOR < 180.0 )
									sun[0] = sun[0] + 360.0*DTOR;
								tmp2 += sun[0];
								minute = 60.0*0.5*( sunset - (double)i );
								solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
								tmp += sun[1];
								tmp2 += sun[0];
								sun[1] = tmp/2.0;    /* Zenith angle */
								sun[0] = tmp2/2.0;   /* Azimuth angle */
								if( sun[0]/DTOR > 360.0 )
									sun[0] = sun[0] - 360.0*DTOR;
							
							}
						}
						else  /* Midnight sun or not a sunrise/sunup sunset hour */
							solarpos(yr,mn,dy,i,minute,lat,lng,tz,sun);
					
						incidence(track_mode,tilt,azimuth,rlim,sun[1],sun[0],-1,-1,angle); /* Calculate incident angle */
					
						double alb;
						if (snow[i] <= 0 || snow[i] >= 150) // outside normal range or 0
							alb = 0.2;
						else
							alb = 0.6; // snow cover days

						double irr[3];
						irr[0]=irr[1]=irr[2] = 0;
						perez( sun[8], dn[i],df[i],alb,angle[0],angle[1],sun[1], irr, 0 ); /* Incident solar radiation */
						poa[i] = irr[0]+fd*(irr[1]+irr[2]);

						if (poa_cutin > 0 && poa[i] < poa_cutin)
							poa[i] = 0;

						if (wind_stow > 0 && wind[i] >= wind_stow)
							poa[i] = 0;

						tpoa[i] = transpoa( poa[i],dn[i],angle[0]);  /* Radiation transmitted thru module cover */
						
					}
				}                       /* End of for i++ loop (24 hours)*/                    /* End of sunup[i] if loop */
					

				
				pvwatts_celltemp celltemp( inoct, height, 1.0 /* 1 hr timestep*/ );
				for (int i=0;i<24;i++)
				{
					double pvt = -999, dc = 0, ac = 0;
					if ( poa[i] > 0 )
					{
						pvt = celltemp( poa[i], wind[i], ambt[i] );
						dc = dcpowr(reftem,watt_spec,pwrdgr,tmloss,tpoa[i],pvt, i_ref);
						ac = dctoac(watt_spec,efffp,dc);
					}

					p_poa[cur_hour] = (ssc_number_t)poa[i];
					p_tcell[cur_hour] = (ssc_number_t)pvt;
					p_dc[cur_hour] = (ssc_number_t)dc;
					p_ac[cur_hour] = (ssc_number_t)ac;

					cur_hour++;
				}

			} /* end of day loop in month */
		} /* end of month loop */

	}
};

DEFINE_MODULE_ENTRY( pvwattsv1, "PVWatts V.1 - integrated hourly weather reader and PV system simulator.", 1 )
