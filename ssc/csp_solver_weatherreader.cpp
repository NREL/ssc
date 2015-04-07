#include "core.h"

#include "csp_solver_core.h"

#include "lib_irradproc.h"

#include "sam_csp_util.h"

enum {
	P_FILENAME,

	P_TRACKMODE,
	P_TILT,
	P_AZIMUTH,

	O_YEAR,
	O_MONTH,
	O_DAY,
	O_HOUR,
	O_MINUTE,

	O_GLOBAL,
	O_BEAM,
	O_DIFFUSE,
	O_TDRY,
	O_TWET,
	O_TDEW,
	O_WSPD,
	O_WDIR,
	O_RHUM,
	O_PRES,
	O_SNOW,
	O_ALBEDO,

	O_POA,

	O_SOLAZI,
	O_SOLZEN,
	O_LAT,
	O_LON,
	O_TZ,
	O_SHIFT,
	O_ELEV,

	// debugging outputs
	D_POABEAM,
	D_POADIFF,
	D_POAGND,


	N_MAX
};

const tcsvarinfo csp_weatherreader_params[] = {
	
	/* DIRECTION    DATATYPE      INDEX       NAME           LABEL                                  UNITS      GROUP    META    DEFAULTVALUE */
	{ TCS_PARAM,   TCS_STRING,   P_FILENAME, "file_name",   "Weather file name on local computer",  "",        "",      "",     "" },

	{ TCS_PARAM,   TCS_NUMBER,   P_TRACKMODE,"track_mode",  "Tracking mode for surface",            "0..2",    "Proc",  "0=fixed,1=1axis,2=2axis", "0" },
	{ TCS_PARAM,   TCS_NUMBER,   P_TILT,     "tilt",        "Tilt angle of surface/axis",           "deg",     "Proc",  "",     "" },
	{ TCS_PARAM,   TCS_NUMBER,   P_AZIMUTH,  "azimuth",     "Azimuth angle of surface/axis",        "deg",     "Proc",  "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_YEAR,     "year",        "Year",                                 "yr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MONTH,    "month",       "Month",                                "mn",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DAY,      "day",         "Day",                                  "dy",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HOUR,     "hour",        "Hour",                                 "hr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MINUTE,   "minute",      "Minute",                               "mi",      "Time",  "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_GLOBAL,   "global",      "Global horizontal irradiance",         "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_BEAM,     "beam",        "Beam normal irradiance",               "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DIFFUSE,  "diff",        "Diffuse horizontal irradiance",        "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TDRY,     "tdry",        "Dry bulb temperature",                 "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TWET,     "twet",        "Wet bulb temperature",                 "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TDEW,     "tdew",        "Dew point temperature",                "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_WSPD,     "wspd",        "Wind speed",                           "m/s",     "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_WDIR,     "wdir",        "Wind direction",                       "deg",     "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_RHUM,     "rhum",        "Relative humidity",                    "%",       "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_PRES,     "pres",        "Pressure",                             "mbar",    "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SNOW,     "snow",        "Snow cover",                           "cm",      "Meteo", "valid (0,150)",   "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ALBEDO,   "albedo",      "Ground albedo",                        "0..1",    "Meteo", "valid (0,1)",     "" },
	
	{ TCS_OUTPUT,  TCS_NUMBER,   O_POA,      "poa",         "Plane-of-array total incident irradiance", "W/m2","Irrad", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLAZI,   "solazi",      "Solar Azimuth",                        "deg",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLZEN,   "solzen",      "Solar Zenith",                         "deg",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LAT,      "lat",         "Latitude",                             "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LON,      "lon",         "Longitude",                            "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TZ,       "tz",          "Timezone",                             "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SHIFT,    "shift",       "shift in longitude from local standard meridian", "deg", "Solar", "", "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ELEV,     "elev",        "Site elevation",                       "m",       "Meteo", "",     "" },

	{ TCS_DEBUG,   TCS_NUMBER,   D_POABEAM,  "poa_beam",    "Plane-of-array beam irradiance",       "W/m2",    "Irrad", "",     "" },
	{ TCS_DEBUG,   TCS_NUMBER,   D_POADIFF,  "poa_diff",    "Plane-of-array diffuse irradiance",    "W/m2",    "Irrad", "",     "" },
	{ TCS_DEBUG,   TCS_NUMBER,   D_POAGND,   "poa_gnd",     "Plane-of-array ground irradiance",     "W/m2",    "Irrad", "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


C_csp_weatherreader::C_csp_weatherreader()
{
	set_params_and_size_vector(csp_weatherreader_params);
}

void C_csp_weatherreader::init()
{
	std::string file = value_str(P_FILENAME).c_str();
	if( !m_wf.open( file ) )
	{
		char tstr[300];
		sprintf(tstr, "Could not open %s for reading", file.c_str());
		throw exec_error("Weather file reader initialization", tstr);
	}
	m_first = true;		// True the first time call() is accessed
}

void C_csp_weatherreader::timestep_call(double time, double step)
{
	//If the start time does not correspond to the first record in the weather file, loop to the correct record
	int nread = 1;
	if( m_first )
	{
		nread = (int)time / step;
		m_first = false;
	}

	for( int i = 0; i<nread; i++ )		//for all calls except the first, nread=1
	{
		if( !m_wf.read() )
		{
			char tstr[300];
			sprintf(tstr, "failed to read from weather file %s at time %lg", m_wf.filename().c_str(), time);
			throw exec_error("Weather file timestep error", tstr);
			//message(TCS_ERROR, "failed to read from weather file %s at time %lg", m_wf.filename().c_str(), time);
			//return -1; // error code
		}
	}

	int trackmode = (int)value(P_TRACKMODE);
	if( trackmode < 0 || trackmode > 2 )
	{
		char tstr[300];
		sprintf(tstr, "invalid tracking mode specified %d [0..2]", trackmode);
		throw exec_error("Weather file timestep error", tstr);
		//message(TCS_ERROR, "invalid tracking mode specified %d [0..2]", trackmode);
		//return -1;
	}

	double tilt = value(P_TILT);
	double azimuth = value(P_AZIMUTH);

	double sunn[9], angle[5], poa[3], diffc[3];

	poa[0] = poa[1] = poa[2] = 0;
	angle[0] = angle[1] = angle[2] = angle[3] = angle[4] = 0;
	diffc[0] = diffc[1] = diffc[2] = 0;

	solarpos(m_wf.year, m_wf.month, m_wf.day, m_wf.hour, m_wf.minute,
		m_wf.lat, m_wf.lon, m_wf.tz, sunn);

	if( sunn[2] > 0.0087 )
	{
		/* sun elevation > 0.5 degrees */
		incidence(trackmode, tilt, azimuth, 45.0, sunn[1], sunn[0], 0, 0, angle);
		perez(sunn[8], m_wf.dn, m_wf.df, 0.2, angle[0], angle[1], sunn[1], poa, diffc);
	}

	// set some output values
	value(O_YEAR, m_wf.year);
	value(O_MONTH, m_wf.month);
	value(O_DAY, m_wf.day);
	value(O_HOUR, m_wf.hour);
	value(O_MINUTE, m_wf.minute);

	value(O_GLOBAL, m_wf.gh);
	value(O_BEAM, m_wf.dn);
	value(O_DIFFUSE, m_wf.df);
	value(O_TDRY, m_wf.tdry);
	value(O_TWET, m_wf.twet);
	value(O_TDEW, m_wf.tdew);
	value(O_WSPD, m_wf.wspd);
	value(O_WDIR, m_wf.wdir);
	value(O_RHUM, m_wf.rhum);
	value(O_PRES, m_wf.pres);
	value(O_SNOW, m_wf.snow);
	value(O_ALBEDO, m_wf.albedo);

	value(O_POA, poa[0] + poa[1] + poa[2]);
	value(O_SOLAZI, sunn[0] * 180 / CSP::pi);
	value(O_SOLZEN, sunn[1] * 180 / CSP::pi);
	value(O_LAT, m_wf.lat);
	value(O_LON, m_wf.lon);
	value(O_TZ, m_wf.tz);
	value(O_SHIFT, (m_wf.lon - m_wf.tz*15.0));
	value(O_ELEV, m_wf.elev);

	value(D_POABEAM, poa[0]);
	value(D_POADIFF, poa[1]);
	value(D_POAGND, poa[2]);
}