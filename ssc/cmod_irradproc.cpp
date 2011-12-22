#include "core.h"
#include "lib_util.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_irradproc[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                              UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",                 "Irradiance input mode",           "0/1",   "Beam+Diff,Global+Beam",  "Irradiance Processor",      "?=0",                     "INTEGER,MIN=0,MAX=1", ""},

	{ SSC_INPUT,        SSC_ARRAY,       "beam",                       "Beam normal irradiance",          "W/m2",   "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diffuse",                    "Diffuse horizontal irradiance",   "W/m2",   "",                      "Irradiance Processor",      "irrad_mode=0",             "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_ARRAY,       "global",                     "Global horizontal irradiance",    "W/m2",   "",                      "Irradiance Processor",      "irrad_mode=1",              "LENGTH_EQUAL=beam",     "" },

	{ SSC_INPUT,        SSC_ARRAY,       "albedo",                     "Ground reflectance (time depend.)","frac",  "0..1",                   "Irradiance Processor",      "?",                        "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo_const",               "Ground reflectance (single value)","frac",  "0..1",                   "Irradiance Processor",      "?=0.2",                    "",                      "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "year",                       "Year",                             "yr",     "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "month",                      "Month",                            "mn",     "1-12",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "day",                        "Day",                              "dy",     "1-days in month",                 "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "hour",                       "Hour",                             "hr",     "0-23",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "minute",                     "Minute",                           "min",    "0-59",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },

	
	{ SSC_INPUT,        SSC_NUMBER,      "lat",                        "Latitude",                         "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lon",                        "Longitude",                        "deg",    "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tz",                         "Time zone",                        "hr",     "",                      "Irradiance Processor",      "*",                        "",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                  "Tilted surface irradiance model", "0/1/2", "Isotropic,HDKR,Perez",  "Irradiance Processor",      "?=2",                     "INTEGER,MIN=0,MAX=2", ""},

	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "Irradiance Processor",      "*",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "Irradiance Processor",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "Irradiance Processor",      "?",                       "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                     "Rotational limit on tracker",    "deg",    "",                      "Irradiance Processor",      "?=45",                    "MIN=0,MAX=90",                             "" },
	
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_beam",                   "Incident Beam Irradiance",       "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff",                "Incident Sky Diffuse",           "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_gnddiff",                "Incident Ground Reflected Diffuse", "W/m2", "",                     "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_iso",            "Incident Diffuse Isotropic Component", "W/m2", "",                  "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_cir",            "Incident Diffuse Circumsolar Component", "W/m2", "",                "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff_hor",            "Incident Diffuse Horizon Brightening Component", "W/m2", "",        "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "incidence",                  "Incidence angle to surface",     "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "surf_tilt",                  "Surface tilt angle",             "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "surf_azm",                   "Surface azimuth angle",          "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "axis_rotation",              "Tracking axis rotation angle",   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_azm",                    "Solar azimuth",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_zen",                    "Solar zenith",                   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_elv",                    "Sun elevation",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_dec",                    "Sun declination",                "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },


var_info_invalid };

class cm_irradproc : public compute_module
{
public:
	cm_irradproc()
	{
		add_var_info( _cm_vtab_irradproc );
	}

	void exec( ) throw( general_error )
	{
		size_t count;
		ssc_number_t *beam = as_array("beam", &count);

		if (count < 2) throw general_error("need at least 2 data points in irradproc");
		
		ssc_number_t *diff = 0, *glob = 0;
		int irrad_mode = as_integer("irrad_mode");
		if ( irrad_mode == 1 )
			glob = as_array("global", &count );
		else
			diff = as_array("diffuse", &count );

		ssc_number_t *year = as_array("year", &count);		
		ssc_number_t *month = as_array("month", &count);
		ssc_number_t *day = as_array("day", &count);
		ssc_number_t *hour = as_array("hour", &count);
		ssc_number_t *minute = as_array("minute", &count);

		int sky_model = as_integer("sky_model");

		double lat = as_double("lat");
		double lon = as_double("lon");
		double tz = as_double("tz");

		double tilt = lat;
		if (is_assigned("tilt"))
			tilt = as_double("tilt");

		double azimuth = as_double("azimuth");
		int track_mode = as_integer("track_mode");
		double rotlim = as_double("rotlim");

		double alb_const = as_double("albedo_const");
		ssc_number_t *albvec = 0;
		if (is_assigned("albedo")) albvec = as_array("albedo", &count);
	
		double sun[9],poa[3],angle[4],diffc[3];
		
		// allocate outputs
		ssc_number_t *p_inc = allocate("incidence", count);
		ssc_number_t *p_surftilt = allocate("surf_tilt", count);
		ssc_number_t *p_surfazm = allocate("surf_azm", count);
		ssc_number_t *p_rot = allocate("axis_rotation", count);
		ssc_number_t *p_azm = allocate("sun_azm", count);
		ssc_number_t *p_zen = allocate("sun_zen", count);
		ssc_number_t *p_elv = allocate("sun_elv", count);
		ssc_number_t *p_dec = allocate("sun_dec", count);

		ssc_number_t *p_poa_beam = allocate("poa_beam", count);
		ssc_number_t *p_poa_skydiff = allocate("poa_skydiff", count);
		ssc_number_t *p_poa_gnddiff = allocate("poa_gnddiff", count);
		
		ssc_number_t *p_poa_skydiff_iso = allocate("poa_skydiff_iso", count);
		ssc_number_t *p_poa_skydiff_cir = allocate("poa_skydiff_cir", count);
		ssc_number_t *p_poa_skydiff_hor = allocate("poa_skydiff_hor", count);


		// "temporary" debugging output
		ssc_number_t *p_sunup = allocate("sunup", count);
		ssc_number_t *p_sunrise = allocate("sunrise", count);
		ssc_number_t *p_sunset = allocate("sunset", count);
		ssc_number_t *p_delt = allocate("delt", count);
		ssc_number_t *p_time = allocate("tcur", count);
		ssc_number_t *p_hrc = allocate("hr_calc", count);
		ssc_number_t *p_minc = allocate("min_calc", count);

		for (size_t i = 0; i < count ;i ++ )
		{
			double t_cur = hour[i] + minute[i]/60.0;
			double delt = 1.0;
			if ( i == 0 )
			{
				double t_next = hour[i+1] + minute[i+1]/60.0;
				if (t_cur > t_next) t_next += 24;
				delt = t_next - t_cur;
			}
			else
			{
				double t_prev = hour[i-1] + minute[i-1]/60.0;
				if (t_cur < t_prev) t_cur += 24;
				delt = t_cur - t_prev;
			}

			if (delt > 2.0) throw general_error( util::format("timestep at index %s yielded a calculated timestep of %lg.  2 hour maximum timestep allowed.", i, delt ));

			p_time[i] = (ssc_number_t)t_cur;
			p_delt[i] = (ssc_number_t)delt;
						
			// calculate sunrise and sunset hours in local standard time for the current day
			solarpos( (int)year[i], (int)month[i], (int)day[i], 12, 0.0, lat, lon, tz, sun );

			double t_sunrise = sun[4];
			double t_sunset = sun[5];

			if ( t_cur >= t_sunrise - delt/2.0
				&& t_cur < t_sunrise + delt/2.0 )
			{
				// time step encompasses the sunrise
				double t_calc = (t_sunrise + (t_cur+delt/2.0))/2.0; // midpoint of sunrise and end of timestep
				int hr_calc = (int)t_calc;
				double min_calc = (t_calc-hr_calc)*60.0;

				p_hrc[i] = (ssc_number_t) hr_calc;
				p_minc[i] = (ssc_number_t) min_calc;
				
				solarpos( (int)year[i], (int)month[i], (int)day[i], hr_calc, min_calc, lat, lon, tz, sun );

				p_sunup[i] = 2;				
			}
			else if (t_cur > t_sunset - delt/2.0
				&& t_cur <= t_sunset + delt/2.0 )
			{
				// timestep encompasses the sunset
				double t_calc = ( (t_cur-delt/2.0) + t_sunset )/2.0; // midpoint of beginning of timestep and sunset
				int hr_calc = (int)t_calc;
				double min_calc = (t_calc-hr_calc)*60.0;

				p_hrc[i] = (ssc_number_t) hr_calc;
				p_minc[i] = (ssc_number_t) min_calc;
				
				solarpos( (int)year[i], (int)month[i], (int)day[i], hr_calc, min_calc, lat, lon, tz, sun );

				p_sunup[i] = 3;
			}
			else if (t_cur >= t_sunrise && t_cur <= t_sunset)
			{
				// timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)			
				p_hrc[i] = hour[i];
				p_minc[i] = minute[i];
				solarpos( (int)year[i], (int)month[i], (int)day[i], (int)hour[i], minute[i], lat, lon, tz, sun );
				p_sunup[i] = 1;
			}
			else
			{			
				p_hrc[i] = -1;
				p_minc[i] = -1;
				p_sunup[i] = 0;
			}

			
			poa[0]=poa[1]=poa[2] = 0;
			diffc[0]=diffc[1]=diffc[2] = 0;

			angle[0]=angle[1]=angle[2]=angle[3] = 0;

			// do irradiance calculations if sun is up
			if (p_sunup[i] > 0)
			{
				double alb = alb_const;
				// if we have array of albedo values, use it
				if ( albvec != 0  && albvec[i] >= 0 && albvec[i] <= (ssc_number_t)1.0)
					alb = albvec[i];
				
				// compute incidence angles onto fixed or tracking surface
				incidence( track_mode, tilt, azimuth, rotlim, sun[1], sun[0], angle );


				double hextra = sun[8];
				double hbeam = beam[i]*cos( sun[1] ); // calculated beam on horizontal surface: sun[1]=zenith
				
				// check beam irradiance against extraterrestrial irradiance
				if ( hbeam > hextra )
				{
					log( util::format("beam irradiance on horizontal of %lg W/m2 exceeded calculated extraterrestrial irradiance of %lg W/m2 constant at %d mon %d day %d hr %lg min",
							(double)hbeam, hextra, (int)month[i], (int)day[i], (int)hour[i], (double)minute[i], i), SSC_WARNING, (float)i ); 
				}


				// compute beam and diffuse inputs based on irradiance inputs mode
				double ibeam = beam[i];
				double idiff = 0.0;
				if ( irrad_mode == 0 && diff != 0 )  // Beam+Diffuse
					idiff = diff[i];
				else if ( irrad_mode == 1 && glob != 0 ) // Total+Beam
					idiff = glob[i] - hbeam;
				else
					throw general_error("diffuse or global must be given in additional to beam irradiance");

				// compute incident irradiance on tilted surface
				switch( sky_model )
				{
				case 0:
					isotropic( sun[8], ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
					break;
				case 1:
					hdkr( sun[8], ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
					break;
				default:
					perez( sun[8], ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
					break;
				}
			}
			
			// assign outputs
			p_inc[i] = (ssc_number_t) (angle[0] * 180/M_PI);
			p_surftilt[i] = (ssc_number_t) (angle[1] * 180/M_PI);
			p_surfazm[i] = (ssc_number_t) (angle[2] * 180/M_PI);
			p_rot[i] = (ssc_number_t) (angle[3] * 180/M_PI);
			p_azm[i] = (ssc_number_t) (sun[0] * 180/M_PI);
			p_zen[i] = (ssc_number_t) (sun[1] * 180/M_PI);
			p_elv[i] = (ssc_number_t) (sun[2] * 180/M_PI);
			p_dec[i] = (ssc_number_t) (sun[3] * 180/M_PI);			
			p_poa_beam[i] = (ssc_number_t) poa[0];
			p_poa_skydiff[i] = (ssc_number_t) poa[1];
			p_poa_gnddiff[i] = (ssc_number_t) poa[2];
			
			
			p_poa_skydiff_iso[i] = (ssc_number_t) diffc[1];
			p_poa_skydiff_cir[i] = (ssc_number_t) diffc[1];
			p_poa_skydiff_hor[i] = (ssc_number_t) diffc[1];

			p_sunrise[i] = (ssc_number_t) sun[4];
			p_sunset[i] = (ssc_number_t) sun[5];
		}
	}
};

DEFINE_MODULE_ENTRY( irradproc, "Irradiance Processor", 1 )
