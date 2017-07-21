/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/
#define _TCSTYPEINTERFACE_
#include "tcstype.h"

#include <shared/lib_weatherfile.h>
#include <shared/lib_irradproc.h>

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif




enum {	P_FILENAME, 

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


		N_MAX };

tcsvarinfo weatherreader_variables[] = {
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

class weatherreader : public tcstypeinterface
{
private:
	weatherfile m_wfile;
	weather_header m_hdr;
	weather_record m_rec;

	bool m_first;	//flag to indicate whether this is the first call
public:
	weatherreader( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti ) { }

	virtual ~weatherreader() { }

	virtual int init()
	{
		std::string file = value_str( P_FILENAME ).c_str();
		if (! m_wfile.open( file ) )
		{
			message(TCS_ERROR, "could not open %s for reading", file.c_str() );
			return -1;
		}

		m_wfile.header( &m_hdr );

		m_first = true; //True the first time call() is accessed
		return 0; // success
	}

	virtual int call( double time, double step, int ncall )
	{
		if ( ncall == 0 ) // only read data values once per timestep
		{
			//If the start time does not correspond to the first record in the weather file, loop to the correct record
			int nread=1;
			if(m_first){
				nread = (int)time/step;
				m_first = false;
			}

			for(int i=0; i<nread; i++){		//for all calls except the first, nread=1
				
				if ( !m_wfile.read( &m_rec ) )
				{
					message(TCS_ERROR, "failed to read from weather file %s at time %lg", m_wfile.filename().c_str(), time );
					return -1; // error code
				}

			}

		}

		int trackmode = (int) value( P_TRACKMODE );
		if (trackmode < 0 || trackmode > 2)
		{
			message(TCS_ERROR, "invalid tracking mode specified %d [0..2]", trackmode);
			return -1;
		}

		double tilt = value( P_TILT );
		double azimuth = value( P_AZIMUTH );

		double sunn[9], angle[5], poa[3], diffc[3];
		
		poa[0] = poa[1] = poa[2] = 0;
		angle[0] = angle[1] = angle[2] = angle[3] = angle[4] = 0;
		diffc[0] = diffc[1] = diffc[2] = 0;
	
		solarpos( m_rec.year, m_rec.month, m_rec.day, m_rec.hour, m_rec.minute,
			m_hdr.lat, m_hdr.lon, m_hdr.tz, sunn );

		if (sunn[2] > 0.0087)
		{
			/* sun elevation > 0.5 degrees */
			incidence( trackmode, tilt, azimuth, 45.0, sunn[1], sunn[0], 0, 0, angle );
			perez( sunn[8], m_rec.dn, m_rec.df, 0.2, angle[0], angle[1], sunn[1], poa, diffc );
		}
		
		// set some output values
		value( O_YEAR, m_rec.year );
		value( O_MONTH, m_rec.month );
		value( O_DAY, m_rec.day );
		value( O_HOUR, m_rec.hour );
		value( O_MINUTE, m_rec.minute );

		value( O_GLOBAL, m_rec.gh );
		value( O_BEAM, m_rec.dn );
		value( O_DIFFUSE, m_rec.df );
		value( O_TDRY, m_rec.tdry );
		value( O_TWET, m_rec.twet );
		value( O_TDEW, m_rec.tdew );
		value( O_WSPD, m_rec.wspd );
		value( O_WDIR, m_rec.wdir );
		value( O_RHUM, m_rec.rhum );
		value( O_PRES, m_rec.pres );
		value( O_SNOW, m_rec.snow );
		value( O_ALBEDO, m_rec.alb );

		value( O_POA,  poa[0]+poa[1]+poa[2] );
		value( O_SOLAZI, sunn[0]*180/M_PI );
		value( O_SOLZEN, sunn[1]*180/M_PI );
		value( O_LAT, m_hdr.lat );
		value( O_LON, m_hdr.lon );
		value( O_TZ, m_hdr.tz );
		value( O_SHIFT, (m_hdr.lon - m_hdr.tz*15.0));
		value( O_ELEV, m_hdr.elev );

		value( D_POABEAM, poa[0] );
		value( D_POADIFF, poa[1] );
		value( D_POAGND, poa[2] );

		return 0; // success
	}
};

TCS_IMPLEMENT_TYPE( weatherreader, "Standard Weather File format reader", "Aron Dobos", 1, weatherreader_variables, NULL, 0 )

