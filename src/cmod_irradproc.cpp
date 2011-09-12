#include "core.h"

#include "lib_wfhrly.h"

static var_info _cm_vtab_irradproc[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "beam",                       "Beam irradiance",                  "W/m2",   "",                      "Irradiance Processor",      "?",                        "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diffuse",                    "Diffuse irradiance",               "W/m2",   "",                      "Irradiance Processor",      "?",                        "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "time",                       "Time",                             "sec",    "",                      "Irradiance Processor",      "?",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "year",                       "Year",                             "yr",     "",                      "Irradiance Processor",      "?",                        "",                      "" },
	{ SSC_INPUT,        SSC_STRING,      "file_name",                  "Weather file path",                "",       "",                      "Irradiance Processor",      "?",                        "LOCAL_FILE",            "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",                  "Tilted surface irradiance model", "0/1/2", "Isotropic,HDKR,Perez",  "Irradiance Processor",      "?=2",                     "INTEGER,MIN=0,MAX=2", ""},

	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "Irradiance Processor",      "*",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "Irradiance Processor",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "Irradiance Processor",      "?",                       "MIN=0,MAX=90",                             "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "lat",                        "Latitude",                       "deg",    "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lon",                        "Longitude",                      "deg",    "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tz",                         "Timezone",                       "tz",     "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "elev",                       "Elevation",                      "m",      "",                      "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_beam",                   "Incident Beam Irradiance",       "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_skydiff",                "Incident Sky Diffuse",           "W/m2",   "",                      "Irradiance Processor",      "*",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_gnddiff",                "Incident Ground Reflected Diffuse", "W/m2", "",                     "Irradiance Processor",      "*",                       "",                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "t_dry",                      "Temperature, dry bulb",          "C",   "",                         "Irradiance Processor",      "?",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "t_wet",                      "Temperature, wet bulb",          "C",   "",                         "Irradiance Processor",      "?",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "relhum",                     "Relative humidity",              "%", "",                           "Irradiance Processor",      "?",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pres",                       "Atmospheric pressure",           "mbar", "",                        "Irradiance Processor",      "?",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "w_spd",                      "Wind speed",                     "m/s", "",                         "Irradiance Processor",      "?",                       "",                  "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "w_dir",                      "Wind direction",                 "deg", "",                         "Irradiance Processor",      "?",                       "",                  "" },


var_info_invalid };