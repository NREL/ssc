#include "core.h"
#include "lib_util.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_irradproc[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                              UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	
	{ SSC_INPUT,        SSC_ARRAY,       "beam",                       "Beam irradiance",                  "W/m2",   "",                      "Irradiance Processor",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diffuse",                    "Diffuse irradiance",               "W/m2",   "",                      "Irradiance Processor",      "*",                        "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_ARRAY,       "albedo",                     "Ground reflectance (time depend.)","frac",  "0..1",                   "Irradiance Processor",      "?",                        "LENGTH_EQUAL=beam",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo_const",               "Ground reflectance (single value)","frac",  "0..1",                   "Irradiance Processor",      "?=0.2",                    "",                      "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "year",                       "Year",                             "yr",     "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",               "" },
	{ SSC_INPUT,        SSC_ARRAY,       "month",                      "Month",                            "mn",     "1-12",                  "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "day",                        "Day",                              "dy",     "1-365",                 "Irradiance Processor",      "*",                       "LENGTH_EQUAL=beam",                          "" },
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

	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_azm",                    "Solar azimuth",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_zen",                    "Solar zenith",                   "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_elv",                    "Sun elevation",                  "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=dn",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sun_dec",                    "Sun declination",                "deg",    "",                      "Irradiance Processor",      "*",                       "LENGTH_EQUAL=dn",                          "" },


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
		ssc_number_t *diff = as_array("diff", &count);

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

		double alb = as_double("albedo_const");
		ssc_number_t *albvec = 0;
		if (is_assigned("albedo")) albvec = as_array("albedo", &count);
	
		double sun[9],poa[3],angle[3];
		
		// allocate outputs
		ssc_number_t *p_azm = allocate("sun_azm", count);
		ssc_number_t *p_zen = allocate("sun_zen", count);
		ssc_number_t *p_elv = allocate("sun_elv", count);
		ssc_number_t *p_dec = allocate("sun_dec", count);
		ssc_number_t *p_poa_beam = allocate("poa_beam", count);
		ssc_number_t *p_poa_skydiff = allocate("poa_skydiff", count);
		ssc_number_t *p_poa_gnddiff = allocate("poa_gnddiff", count);

		for (size_t i = 0; i < count ;i ++ )
		{
			// calculate solar position
			solarpos( (int)year[i], (int)month[i], (int)day[i], (int)hour[i], minute[i], lat, lon, tz, sun );
				
			poa[0]=poa[1]=poa[2] = 0;

			// if sun is above horizon at this time
			if (sun[2] > 0)
			{
				// if we have array of albedo values, use it
				if ( albvec != 0 ) alb = albvec[i];  
				
				// compute incidence angles depending on tracking surface
				incidence( track_mode, tilt, azimuth, rotlim, sun[1], sun[0], angle );

				// compute incident irradiance on tilted surface
				switch( sky_model )
				{
				case 0:
					isotropic( sun[8], beam[i], diff[i], alb, angle[0], angle[1], sun[1], poa );
					break;
				case 1:
					hdkr( sun[8], beam[i], diff[i], alb, angle[0], angle[1], sun[1], poa );
					break;
				default:
					perez( sun[8], beam[i], diff[i], alb, angle[0], angle[1], sun[1], poa );
					break;
				}
			}
			
			// assign outputs
			p_azm[i] = (ssc_number_t) (sun[0] * 180/M_PI);
			p_zen[i] = (ssc_number_t) (sun[1] * 180/M_PI);
			p_elv[i] = (ssc_number_t) (sun[2] * 180/M_PI);
			p_dec[i] = (ssc_number_t) (sun[3] * 180/M_PI);			
			p_poa_beam[i] = (ssc_number_t) poa[0];
			p_poa_skydiff[i] = (ssc_number_t) poa[1];
			p_poa_gnddiff[i] = (ssc_number_t) poa[2];
		}
	}
};

DEFINE_MODULE_ENTRY( irradproc, "Irradiance Processor", 1 )
