#include <stdio.h>
#include <cmath>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <numeric>
#include <limits>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)
#define CASECMP(a,b) _stricmp(a,b)
#define CASENCMP(a,b,n) _strnicmp(a,b,n)
#else
#define CASECMP(a,b) strcasecmp(a,b)
#define CASENCMP(a,b,n) strncasecmp(a,b,n)
#endif

#include "lib_util.h"
#include "lib_weatherfile.h"






#ifdef _MSC_VER
#define my_isnan(x) ::_isnan( x )
#else
#define my_isnan(x) std::isnan( x )
#endif

static void trimnlcr( char *buf )
{
	if (!buf) return;

	size_t len = strlen(buf);
	if (len > 0 && buf[len-1] == '\n') // strip newline
		buf[len-1] = 0;

	if (len > 1 && buf[len-2] == '\r') // strip carriage return
		buf[len-2] = 0;
}

static char *trimboth( char *buf )
{
	if (!buf) return 0;

	size_t len = strlen(buf);
	if ( len == 0 ) return buf;

	char *p = buf+len-1;
	while ( p > buf && p && *p 
		&& (*p == '\n' || *p == '\r' || *p == ' ' || *p == '\t') )
	{
		*p = 0;
		p--;
	}
	
	p = buf;
	while ( p && *p && *p == ' ' || *p == '\t' )
		p++;

	return p;
}

static int locate(char *buf, char **colidx, int colmax, char delim)
{
	trimnlcr(buf);

	char *p = buf;
	int i = 1;
	int ncols = 0;
	
	colidx[0] = p;
	while (p && *p && i < colmax)
	{
		p = strchr(p, delim);
		if (p)
		{
			*p = 0;
			colidx[i++] = ++p;
		}
	}

	ncols = i;

	while (i<colmax) colidx[i++] = 0;

	return ncols;
}

static double conv_deg_min_sec(double degrees, 
								double minutes, 
								double seconds, 
								char direction)
{
	double dd = degrees + minutes/60.0 + seconds/3600.0;
	if ( tolower((int)direction) == 's' || tolower((int)direction) == 'w')
		dd = 0 - dd;
	return dd;
}


/* 
   version of strtok_r from (2010/9/24)
   http://www.koders.com/c/fid9E7961E1E818E911DA7C34DD56DD8782726B3FCD.aspx
   */
char *gettoken (char *s, const char *delim, char **save_ptr)
{
	char *token;

	if (s == NULL)
		s = *save_ptr;

	/* Scan leading delimiters.  */
	s += strspn (s, delim);
	if (*s == '\0')
	{
		*save_ptr = s;
		return NULL;
	}

	/* Find the end of the token.  */
	token = s;
	s = strpbrk (token, delim);
	
	if (s == NULL)
		/* This token finishes the string.  */
		*save_ptr = strchr (token, '\0');
	else
	{
		/* Terminate the token and make *SAVE_PTR point past it.  */
		*s = '\0';
		*save_ptr = s + 1;
	}
	return token;
}

static int cmp_ext(const char *file, const char *ext)
{
	size_t len_ext, len_file;
	const char *extp;
	
	if (!file || !ext) return 0;
	len_ext = strlen(ext);
	len_file = strlen(file);
	extp = file+len_file-len_ext;
	
	if (extp < file) return 0;
	return CASENCMP(extp, ext, len_ext)==0 ? 1 : 0;
}


std::string weatherfile::normalize_city( const std::string &in )
{
	std::string city = util::lower_case( in );
	util::replace( city, "_", " " );
	util::replace( city, "\"", "" );
	util::replace( city, "/", " " );
	util::replace( city, "\\", " " );

	for ( size_t i=0;i<city.length();i++ )
	{
		if ( i==0 || city[i-1] == ' ' )
			city[i] = toupper( city[i] );
	}
	return city;
}

static double calc_twet( double T, double RH, double P )
{
//	function [Twet] = calctwet(T, RH, P)
//% calculate wet bulb temperature from T (dry bulb, 'C), RH (%), Pressure
//% (mbar)
//% see http://www.ejournal.unam.mx/atm/Vol07-3/ATM07304.pdf for eqns.

	/* 
	Mike Wagner:
	There is a units error here! The original reference specifies that pressure should be provided in 
	hPa (hectoPascals), which is equivalent with millibar. However, the units SHOULD BE in kPa, or mbar/10.
	Correct for the units issue here.

	IMPACT:
	This subroutine has been returning wet bulb temperatures much too high. This could adversely affect any
	model that calls the method and whose performance is sensitive to the wet bulb temperature.
	*/
	volatile double Pkpa = P/10.;	//Correct for units problem

    //volatile double Twet = T*0.7;// initial guess
	volatile double Twet = T - 5.;	//Initial guess [mjw -- negative values of T were causing problems here]
	
	//[mjw] Use a bisection method to solve for Twet. The previous iteration method is unstable.
	bool
		hiflag = false,
		lowflag = false;
	double
		hival, lowval, err;
	const double tol = 0.05;

    int i = 0;
    while( i++ < 250 )
	{
        err = exp( (21.3 * Twet + 494.41) / (Twet+273.15) ) - RH / 100 * exp( (21.3 * T + 494.41) / (T+273.15) ) - (6.53*10e-4) * Pkpa * (T-Twet);
        //double G = exp( (21.3 * Twet + 494.41) / (Twet+273.15) ) * ( (21.4 * (Twet+273.15) - (21.4 * Twet+494.41)) / pow(Twet+273.15, 2) ) + 6.53*10e-4 * Pkpa * Twet;
		if(err < 0.){
			lowval = Twet;
			lowflag = true;
		}
		else if(err > 0.){
			hival = Twet;
			hiflag = true;
		}
		
		if (fabs(err) < tol) break;

		//If the error is still too high, guess new values
		if(hiflag && lowflag){
			//Bisect
			Twet = (hival + lowval)/2.;
		}
		else if(hiflag){
			//A lower bound hasn't yet been found. Try decreasing by 5 C
			Twet += -5;
		}
		else if(lowflag){
			//An upper bound hasn't yet been found. Bisect the current Twet and the Tdry
			Twet = (Twet + T)/2.;
		}
		else{
			//Neither flags have been set. Guess a lower temp.
			Twet += -5.;
		}
		
	}

	if ( Twet != Twet ) // check for NaN
	{
		/*
		from biopower, Jennie Jorgenson:
		For estimating the dew point (first line of code), I used this very simple relation from wikipedia: http://en.wikipedia.org/wiki/Dew_point#Simple_approximation
		The second line is from a slightly sketchier source (http://www.theweatherprediction.com/habyhints/170/), meteorologist Jeff Haby. His procedure is for temperatures in F. 
		*/

		double dp_est = T - ((1 - RH/100) / 0.05);
		Twet = T - ((T - dp_est) / 3.0);
	}

	return Twet;
}

static double wiki_dew_calc( double T, double RH )
{
	// ref: http://en.wikipedia.org/wiki/Dew_point

	if ( RH > 0 )
	{
		static const double a = 17.271;
		static const double b = 237.7;
		double gamma = a*T/(b+T) + log(RH/100.0);
		if ( a-gamma != 0.0 )
			return b*gamma/(a-gamma);
	}

	// ultra-simple equation (OK as long as RH > 50%)
	return  T - (100-RH)/5;
}


#define NBUF 2048
#define NCOL 128


weatherfile::weatherfile()
{
	reset();
}

weatherfile::weatherfile( const std::string &file, bool header_only, bool interp )
{
	reset();
	m_ok = open( file, header_only, interp );
}

void weatherfile::reset()
{
	m_ok = false;
	m_type = INVALID;
	m_startYear = 1900;
	m_time = 0;
	m_index = 0;
	interpmet = false;
	hasunits = false;
	
	m_type = INVALID;
	m_file.clear();
	m_startYear = 1900;

	location.clear();
	city.clear();
	state.clear();
	tz = lat = lon = elev = std::numeric_limits<double>::quiet_NaN();
	start = step = std::numeric_limits<double>::quiet_NaN();
	nrecords = 0;

	reset_record();
}


void weatherfile::reset_record()
{
	year = month = day = hour = 0;
	minute = std::numeric_limits<double>::quiet_NaN();
	gh = dn = df = wspd = wdir = std::numeric_limits<double>::quiet_NaN();
	tdry = twet = tdew = rhum = pres = snow = albedo = aod = std::numeric_limits<double>::quiet_NaN();
}

bool weatherfile::ok()
{
	return m_ok;
}

int weatherfile::type()
{
	return m_type;
}

std::string weatherfile::filename()
{
	return m_file;
}

bool weatherfile::open( const std::string &file, bool header_only, bool interp )
{
	if (file.empty()) return false;
	
	if (cmp_ext(file.c_str(),"tm2") || cmp_ext(file.c_str(), "tmy2"))
		m_type = TMY2;
	else if ( cmp_ext(file.c_str(), "tm3") || cmp_ext(file.c_str(), "tmy3") )
		m_type = TMY3;
	else if ( cmp_ext(file.c_str(), "csv") )
		m_type = WFCSV;
	else if ( cmp_ext(file.c_str(), "epw") )
		m_type = EPW;
	else if ( cmp_ext(file.c_str(), "smw") )
		m_type = SMW;
	else
		return false;
	
	char buf[NBUF+1], *pbuf, 
		buf1[NBUF+1], *pbuf1, 
		*cols[128], *cols1[128];
	
	
	FILE *fp = fopen(file.c_str(), "r");
	if ( !fp )
	{
		m_type = INVALID;
		return false;
	}
	
	m_startYear = 1900;
	m_time = 1800;	
	
	/* read header information */
	if ( m_type==TMY2 )
	{
	/*  93037 COLORADO_SPRINGS       CO  -7 N 38 49 W 104 43  1881 */
		char slat[10], slon[10];
		char pl[256], pc[256], ps[256];
		int dlat, mlat, dlon, mlon, ielv;
		
		fgets(buf, NBUF, fp);
		sscanf(buf, "%s %s %s %lg %s %d %d %s %d %d %d",
				pl, pc, ps,
				&tz,
				slat,&dlat,&mlat,
				slon,&dlon,&mlon,
				&ielv);
		
		lat = conv_deg_min_sec(dlat, mlat, 0, slat[0]);
		lon = conv_deg_min_sec(dlon, mlon, 0, slon[0]);
		location = pl;
		city = pc;
		state = ps;
		elev = ielv;
		start = 1800;
		step = 3600;
		nrecords = 8760;
	}
	else if( m_type==TMY3 )
	{
	/*  724699,"BROOMFIELD/JEFFCO [BOULDER - SURFRAD]",CO,-7.0,40.130,-105.240,1689 */
		char *lasts, *p;
		
		fgets(buf, NBUF, fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		location = p!=0 ? p : "";
				
		p = gettoken( NULL, ",", &lasts);		
		city = p!=0 ? p : "";
		
		p = gettoken(NULL, ",", &lasts);		
		state = p!=0 ? p : "";
		
		p = gettoken(NULL, ",", &lasts);
		tz = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		lat = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		lon = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		elev = atof( p);
		
		
		start = 1800;
		step = 3600;
		nrecords = 8760;

		fgets(buf, NBUF, fp); /* skip over labels line */
	}
	else if( m_type==EPW )
	{
	/*  LOCATION,Cairo Intl Airport,Al Qahirah,EGY,ETMY,623660,30.13,31.40,2.0,74.0 */
	/*  LOCATION,Alice Springs Airport,NT,AUS,RMY,943260,-23.80,133.88,9.5,547.0 */
		char *lasts, *p;
		
		fgets(buf, NBUF, fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		/* skip LOCATION */

		p = gettoken( NULL, ",", &lasts );
		city = p!=0 ? p : "";
		
		p = gettoken( NULL, ",", &lasts );
		state = p!=0 ? p : "";
		
		p = gettoken( NULL, ",", &lasts );
		country = p!=0 ? p: "";
		
		p = gettoken( NULL, ",", &lasts );
		/* skip SOURCE */
		
		p = gettoken( NULL, ",", &lasts );
		location = p!=0 ? p : "";
		
		p = gettoken( NULL, ",", &lasts );
		lat = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		lon = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		tz = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		elev = atof(p);
		
		/* skip over excess header lines */
		
		fgets(buf,NBUF,fp); /* DESIGN CONDITIONS */
		fgets(buf,NBUF,fp); /* TYPICAL/EXTREME PERIODS */
		fgets(buf,NBUF,fp); /* GROUND TEMPERATURES */
		fgets(buf,NBUF,fp); /* HOLIDAY/DAYLIGHT SAVINGS */
		fgets(buf,NBUF,fp); /* COMMENTS 1 */
		fgets(buf,NBUF,fp); /* COMMENTS 2 */
		fgets(buf,NBUF,fp); /* DATA PERIODS */
		
		start = 1800;
		step = 3600;
		nrecords = 8760;
		
	}
	else if ( m_type==SMW )
	{
		char *lasts, *p;
		
		fgets(buf, NBUF, fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		location = p!=0 ? p : "";

		p = gettoken( NULL, ",", &lasts );
		city = p!=0 ? p : "";
		
		p = gettoken( NULL, ",", &lasts );
		state = p!=0 ? p : "";

		p = gettoken( NULL, ",", &lasts );
		tz = atof(p); // timezone

		p = gettoken( NULL, ",", &lasts );
		lat = atof(p); // latitude
		
		p = gettoken( NULL, ",", &lasts );
		lon = atof(p); // longitude

		p = gettoken( NULL, ",", &lasts );
		elev = atof(p); // elevation
		
		p = gettoken( NULL, ",", &lasts );
		step = atof(p);

		p = gettoken( NULL, ",", &lasts );
		m_startYear = atoi(p); // start year

		year = m_startYear;

		p = gettoken( NULL, ",", &lasts );

		double start_hour = 0;
		double start_min = 30;
		double start_sec = 0;

		start_hour = atoi(p);

		p = strchr(p, ':');
		if (p && *p) p++;
		if (p && *p) start_min = atoi(p);
		
		p = strchr(p, ':');
		if (p && *p) p++;
		if (p && *p) start_sec = atoi(p);
		
		m_time = start_hour*3600 + start_min*60 + start_sec;
		start = m_time;

		nrecords = 0;
		while (fgets(buf, NBUF, fp ) != 0)
			nrecords ++;

		::rewind( fp );
		fgets( buf, NBUF, fp );
	}
	else if ( m_type == WFCSV )
	{
		pbuf = fgets( buf, NBUF, fp );
		int ncols = locate( buf, cols, NCOL, ',' );
		pbuf1 = fgets( buf1, NBUF, fp );
		int ncols1 = locate( buf1, cols1, NCOL, ',' );

		int hdr_step_sec = -1;

		if ( ncols != ncols1 
			|| pbuf != buf 
			|| pbuf1 != buf1 ) return -3; // first two lines must have same number of items

		for ( size_t i=0;i<ncols; i++ )
		{
			std::string name = util::lower_case( trimboth( cols[i] ) );
			char *value = trimboth(cols1[i]);

			if ( name == "lat" || name == "latitude" )
			{
				lat = atof( value );
			}
			else if ( name == "lon" || name == "long" || name == "longitude" || name == "lng" )
			{
				lon = atof( value );
			}
			else if ( name == "tz" || name == "timezone" || name == "time zone" )
			{
				tz = atof( value );
			}
			else if ( name == "el" || name == "elev" || name == "elevation" || name == "site elevation" )
			{
				elev = atof( value );
			}
			else if ( name == "year" )
			{
				m_startYear = atoi( value );
			}
			else if ( name == "id" || name == "location" || name == "location id" || name == "station" || name == "station id" || name == "wban" || name == "wban#" )
			{
				location = value;
			}
			else if ( name == "city" )
			{
				city = value;
			}
			else if ( name == "state" || name == "province" || name == "region" )
			{
				state = value;
			}
			else if ( name == "country" )
			{
				country = value;
			}
			else if ( name == "source" || name == "src" )
			{
				source = value;
			}
			else if ( name == "description" || name == "desc" )
			{
				description = value;
			}
			else if ( name == "url" )
			{
				url = value;
			}
			else if ( name == "hasunits" || name == "units" )
			{
				hasunits = ( util::lower_case( value ) == "yes"  || atoi( value ) != 0 );
			}
			else if ( name == "interpmet" )
			{
				interpmet = ( util::lower_case( value ) == "yes" || atoi( value ) != 0 );
			}
			else if ( name == "step" )
			{
				hdr_step_sec = atoi( value );
			}
		}

		
		
		start = 1800;
		step = 3600;
		nrecords = 8760;
		
		
		fgets( buf, NBUF, fp ); // col names
		if ( hasunits ) fgets( buf, NBUF, fp ); // col units

		nrecords = 0; // figure out how many records there are
		while (fgets(buf, NBUF, fp ) != 0 && strlen(buf) > 0)
			nrecords ++;

		// reposition to where we were
		::rewind( fp );
		fgets( buf, NBUF, fp ); // header names
		fgets( buf, NBUF, fp ); // header values
		
		// now determine timestep as best as possible
		int nmult = nrecords / 8760;

		if ( hdr_step_sec > 0 )
		{  // if explicitly specified in header?
			step = hdr_step_sec;
			start = step/2;
		}
		else if ( nmult*8760 == nrecords )
		{
			// multiple of 8760 records: assume 1 year of data
			step = 3600/nmult;
			start = step/2;
		}
		else
			m_ok = false;
	}
	else
	{	
		fclose( fp );
		return false;
	}

	if ( header_only )
	{
		fclose( fp );
		return true;
	}
	
	// preallocate memory for data
	for( size_t i=0;i<_MAXCOL_;i++)
	{
		m_columns[i].index = -1;
		m_columns[i].data.resize( nrecords, std::numeric_limits<float>::quiet_NaN() );
	}

	if ( m_type == WFCSV )
	{
		// if it's a WFCSV format file, we need to determine which columns of data exist

		pbuf = fgets( buf, NBUF, fp ); // read column names	
		if ( pbuf != buf ) return -4;
		int ncols = locate( buf, cols, NCOL, ',' );

		if ( hasunits )
		{
			pbuf1 = fgets( buf1, NBUF, fp ); // read column units;
			if ( pbuf1 != buf1 ) return -5;
			int ncols1 = locate( buf1, cols1, NCOL, ',' );

			if ( ncols != ncols1 ) return -6;
		}

		// determine columns
		for( size_t i=0;i<ncols;i++ )
		{
			char *name_cstr = trimboth( cols[i] );
			if ( name_cstr && strlen(name_cstr) > 0 )
			{
				std::string lowname = util::lower_case( name_cstr );
			
				if ( lowname == "yr" || lowname == "year" ) m_columns[YEAR].index = i;
				else if ( lowname == "mo" || lowname == "month" ) m_columns[MONTH].index = i;
				else if ( lowname == "day" ) m_columns[DAY].index = i;
				else if ( lowname == "hour" || lowname == "hr" ) m_columns[HOUR].index = i;
				else if ( lowname == "min" || lowname == "minute" ) m_columns[MINUTE].index = i;
				else if ( lowname == "ghi" || lowname == "gh" || lowname == "global" || lowname == "global horizontal" || lowname == "global horizontal irradiance" ) m_columns[GHI].index = i;
				else if ( lowname == "dni" || lowname == "dn" || lowname == "beam" || lowname == "direct normal" || lowname == "direct normal irradiance" ) m_columns[DNI].index = i;
				else if ( lowname == "dhi" || lowname == "df" || lowname == "diffuse" || lowname == "diffuse horizontal" || lowname == "diffuse horizontal irradiance" ) m_columns[DHI].index = i;
				else if ( lowname == "tdry" || lowname == "dry bulb" || lowname == "dry bulb temp" || lowname == "temperature" || lowname == "ambient" || lowname == "ambient temp" ) m_columns[TDRY].index = i;
				else if ( lowname == "twet" || lowname == "wet bulb" || lowname == "wet bulb temperature" ) m_columns[TWET].index = i;
				else if ( lowname == "tdew" || lowname == "dew point" || lowname == "dew point temperature" ) m_columns[TDEW].index = i;
				else if ( lowname == "wspd" || lowname == "wind speed" ) m_columns[WSPD].index = i;
				else if ( lowname == "wdir" || lowname == "wind direction" ) m_columns[WDIR].index = i;
				else if ( lowname == "rh" || lowname == "rhum" && lowname == "relative humidity" || lowname == "humidity" ) m_columns[RH].index = i;
				else if ( lowname == "pres" || lowname == "pressure" ) m_columns[PRES].index = i;
				else if ( lowname == "snow" || lowname == "snow cover" || lowname == "snow depth" ) m_columns[SNOW].index = i;
				else if ( lowname == "alb" || lowname == "albedo" ) m_columns[ALB].index = i;
				else if ( lowname == "aod" || lowname == "aerosol" || lowname == "aerosol optical depth" ) m_columns[AOD].index = i;
			}
		}
	}
	
	m_errorLine = 0;
	for( int i=0;i<nrecords;i++ )
	{
		if ( m_errorLine < 0 )
		{
			m_ok = false;
			break;
		}

		if ( m_type == TMY2 )
		{
			int yr,mn,dy,hr,ethor,etdn;
			int d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21;      /* which of these are used? d3, d10, d15 & d20 */
			int u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21;  /* are any of these ever used?? */
			int w1,w2,w3,w4,w5,w6,w7,w8,w9,w10;     
			char f1[2],f2[2],f3[2],f4[2],f5[2],f6[2],f7[2],f8[2],f9[2],f10[2],f11[2],f12[2],f13[2],f14[2],f15[2],f16[2],f17[2],f18[2],f19[2],f20[2],f21[2];

			char *pret = fgets(buf, NBUF, fp );
		
			int nread = sscanf(buf,
				"%2d%2d%2d%2d"
				"%4d%4d"
				"%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d"
				"%2d%1s%1d%2d%1s%1d%4d%1s%1d%4d%1s%1d%3d%1s%1d%4d%1s%1d%3d%1s%1d"
				"%3d%1s%1d%4d%1s%1d%5d%1s%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%3d%1s%1d%3d%1s%1d%3d%1s%1d%2d%1s%1d\n",
					&yr,&mn,&dy,&hr,
					&ethor, /* extraterrestrial horizontal radiation */
					&etdn, /* extraterrestrial direct normal radiation */
					&d1,f1,&u1, /* GH data value 0-1415 Wh/m2, Source, Uncertainty */
					&d2,f2,&u2, /* DN data value 0-1200 Wh/m2, Source, Uncertainty */
					&d3,f3,&u3, /* DF data value 0-700 Wh/m2, Source, Uncertainty */
					&d4,f4,&u4, /* GH illum data value, Source, Uncertainty */
					&d5,f5,&u5, /* DN illum data value, Source, Uncertainty */
					&d6,f6,&u6, /* DF illum data value, Source, Uncertainty */
					&d7,f7,&u7, /* Zenith illum data value, Source, Uncertainty */
					&d8,f8,&u8, /* Total sky cover */
					&d9,f9,&u9, /* opaque sky cover */
					&d10,f10,&u10, /* dry bulb temp -500 to 500 = -50.0 to 50.0 'C */
					&d11,f11,&u11, /* dew point temp -600 to 300 = -60.0 to 30.0 'C */
					&d12,f12,&u12, /* relative humidity 0-100 */
					&d13,f13,&u13, /* pressure millibars */
					&d14,f14,&u14, /* wind direction */
					&d15,&f15,&u15, // wind speed 0 to 400 = 0.0 to 40.0 m/s
					&d16,&f16,&u16, // visibility
					&d17,&f17,&u17, // ceiling height
					&w1,&w2,&w3,&w4,&w5,&w6,&w7,&w8,&w9,&w10, // present weather
					&d18,&f18,&u18, // precipitable water
					&d19,&f19,&u19, // aerosol optical depth
					&d20,&f20,&u20, // snow depth 0-150 cm
					&d21,&f21,&u21 ); // days since last snowfall 0-88

			m_columns[YEAR].data[i] = yr + 1900;
			m_columns[MONTH].data[i] = mn;
			m_columns[DAY].data[i] = dy;
			m_columns[HOUR].data[i] = hr-1;  // hour goes 0-23, not 1-24
			m_columns[MINUTE].data[i] = 30;
			m_columns[GHI].data[i] = (double)d1*1.0;
			m_columns[DNI].data[i]=(double)d2;           /* Direct radiation */
			m_columns[DHI].data[i]=(double)d3;           /* Diffuse radiation */
			m_columns[TDRY].data[i]=(double)d10/10.0;       /* Ambient dry bulb temperature(C) */
			m_columns[TDEW].data[i] = (double)d11/10.0; /* dew point temp */
			m_columns[WSPD].data[i] =(double)d15/10.0;       /* Wind speed(m/s) */
			m_columns[WDIR].data[i] =(double)d14; /* wind dir */
			m_columns[RH].data[i] = (double)d12;
			m_columns[PRES].data[i] = (double)d13;
			m_columns[SNOW].data[i] = (double)d20;
			m_columns[ALB].data[i] = -999; /* no albedo in TMY2 */
			m_columns[AOD].data[i] = -999; /* no AOD in TMY2 */
			m_columns[TWET].data[i] = calc_twet( tdry, rhum, pres ); /* must calculate wet bulb */

			if (nread!=79 || pret!=buf)
				m_errorLine = -i;
		}
		else if ( m_type == TMY3 )
		{
			char *pret = fgets(buf, NBUF, fp );
			int ncols = locate(buf, cols, NCOL, ',');

			if (ncols < 68)
				return false;
	
			char *p = cols[0];

			m_columns[MONTH].data[i] = atoi( p );
			p = strchr(p, '/');
			if (!p)
				return false;
			p++;
			m_columns[DAY].data[i] = atoi( p );
			p = strchr(p, '/');
			if (!p) return false;
			p++;
			m_columns[YEAR].data[i] = atoi( p );

			m_columns[HOUR].data[i] = atoi( cols[1] ) - 1;  // hour goes 0-23, not 1-24
			m_columns[MINUTE].data[i] = 30;

			m_columns[GHI].data[i] = (double)atof( cols[4] );
			m_columns[DNI].data[i] = (double)atof( cols[7] );
			m_columns[DHI].data[i] = (double)atof( cols[10] );

			m_columns[TDRY].data[i] = (double)atof( cols[31] );
			m_columns[TWET].data[i] = (double)atof( cols[34] );

			m_columns[WSPD].data[i] = (double)atof( cols[46] );
			m_columns[WDIR].data[i] = (double)atof( cols[43] );

			m_columns[RH].data[i] = (double)atof( cols[37] );
			m_columns[PRES].data[i] = (double)atof( cols[40] );
			m_columns[SNOW].data[i] = -999.0; // no snowfall in TMY3
			m_columns[ALB].data[i] = (double)atof( cols[61] );
			m_columns[AOD].data[i] = -999; /* no AOD in TMY3 */
				
			m_columns[TDEW].data[i] = wiki_dew_calc( tdry, rhum );

			if ( pret!=buf )
				m_errorLine = -i;
		}
		else if ( m_type == EPW)
		{
			char *pret = fgets(buf, NBUF, fp);
			int ncols = locate(buf, cols, NCOL, ',');

			if (ncols < 32)
				return false;
			
			m_columns[YEAR].data[i] = atoi(cols[0]);
			m_columns[MONTH].data[i] = atoi(cols[1]);
			m_columns[DAY].data[i] = atoi(cols[2]);
			m_columns[HOUR].data[i] = atoi(cols[3])-1;  // hour goes 0-23, not 1-24;
			m_columns[MINUTE].data[i] = 30;

			m_columns[GHI].data[i] = (double)atof(cols[13]);
			m_columns[DNI].data[i] = (double)atof(cols[14]);
			m_columns[DHI].data[i] = (double)atof(cols[15]);
		
			m_columns[WSPD].data[i] = (double)atof(cols[21]);
			m_columns[WDIR].data[i] = (double)atof(cols[20]);
		
			m_columns[TDRY].data[i] = (double)atof(cols[6]);
			m_columns[TWET].data[i] = (double)atof(cols[7]);
		
			m_columns[RH].data[i] = (double)atof( cols[8] );
			m_columns[PRES].data[i] = (double)atof( cols[9] ) * 0.01; /* convert Pa in to mbar */
			m_columns[SNOW].data[i] = (double)atof( cols[30] ); // snowfall
			m_columns[ALB].data[i] = -999; /* no albedo in EPW file */
			m_columns[AOD].data[i] = -999; /* no AOD in EPW */

			m_columns[TDEW].data[i] = wiki_dew_calc( tdry, rhum );

			if ( pret!=buf )
				m_errorLine = -i;
		}
		else if ( m_type == SMW )
		{
			char *pret = fgets(buf, NBUF, fp);
			int ncols = locate(buf, cols, NCOL, ',');

			if (ncols < 12)
				return false;
		
			double T = m_time;
		
			m_columns[YEAR].data[i] = (float)m_startYear; // start year
			m_columns[MONTH].data[i] = (float)util::month_of( T/3600.0 ); // 1-12
			m_columns[DAY].data[i] = (float)util::day_of_month( month, T/3600.0 ); // 1-nday
			m_columns[HOUR].data[i] = (float)(((int)(T/3600.0))%24);  // hour goes 0-23, not 1-24;
			m_columns[MINUTE].data[i] = (float)fmod(T/60.0, 60.0);      // minute goes 0-59
					

			m_time += step; // increment by step

			m_columns[GHI].data[i] = (float)atof(cols[7]);
			m_columns[DNI].data[i] = (float)atof(cols[8]);
			m_columns[DHI].data[i] = (float)atof(cols[9]);
		
			m_columns[WSPD].data[i] = (float)atof(cols[4]);
			m_columns[WDIR].data[i] = (float)atof(cols[5]);
		
			m_columns[TDRY].data[i] = (float)atof(cols[0]);
			m_columns[TDEW].data[i] = (float)atof(cols[1]);
			m_columns[TWET].data[i] = (float)atof(cols[2]);
		
			m_columns[RH].data[i] = (float)atof( cols[3] );
			m_columns[PRES].data[i] = (float)atof( cols[6] );
			m_columns[SNOW].data[i] = (float)atof( cols[11] );
			m_columns[ALB].data[i] = (float)atof( cols[10] );
			m_columns[AOD].data[i] = -999; /* no AOD in SMW */
		
			if ( pret!=buf )
				m_errorLine = -i;
		}
		else if ( m_type == WFCSV )
		{
			buf[0] = 0;
			fgets( buf, NBUF, fp );
			pbuf = trimboth(buf);
			if ( !pbuf || !*pbuf )
			{
				m_errorLine = -i;
				break;
			}

			int ncols = locate( pbuf, cols, NCOL, ',' );	

			for( size_t k=0;k<_MAXCOL_;k++ )
			{
				if ( m_columns[k].index >= 0
					&& m_columns[k].index < ncols )
				{
					m_columns[k].data[i] = (float)atof( trimboth(cols[ m_columns[k].index ]) );
				}
			}
		}

	}

	fclose( fp );

	if ( m_type == WFCSV )
	{
		// special handling for certain columns that we can calculate from others
		// if the data doesn't exist

		if ( m_columns[TWET].index < 0 
			&& m_columns[TDRY].index >= 0
			&& m_columns[PRES].index >= 0
			&& m_columns[RH].index >= 0 )
		{
			for( size_t i=0;i<nrecords;i++ )
				m_columns[TWET].data[i] = (float) calc_twet( m_columns[TDRY].data[i], m_columns[RH].data[i], m_columns[PRES].data[i] );
		}
		
		if ( m_columns[TDEW].index < 0 
			&& m_columns[TDRY].index >= 0
			&& m_columns[RH].index >= 0 )
		{
			for( size_t i=0;i<nrecords;i++ )
				m_columns[TDEW].data[i] = (float) wiki_dew_calc( m_columns[TDRY].data[i], m_columns[RH].data[i] );
		}

		if ( m_columns[YEAR].index < 0 )
		{
			for( size_t i=0;i<nrecords;i++ )
				m_columns[YEAR].data[i] = (float)m_startYear;
		}
		
		if ( m_columns[MONTH].index < 0 
			&& step == 3600 && nrecords == 8760 )
		{
			for( size_t i=0;i<nrecords;i++ )
				m_columns[MONTH].data[i] = (float)util::month_of( i );
		}
		
		if ( m_columns[DAY].index < 0 
			&& step == 3600 && nrecords == 8760 )
		{
			for( size_t i=0;i<nrecords;i++ )
			{
				int month = util::month_of( i );
				m_columns[DAY].data[i] = (float)util::day_of_month( month, i );
			}
		}
		
		if ( m_columns[HOUR].index < 0 
			&& step == 3600 && nrecords == 8760 )
		{
			for( size_t i=0;i<nrecords;i++ )
			{
				int day = i / 24;
				int start_of_day = day*24;
				m_columns[HOUR].data[i] = (float)(i - start_of_day);
			}
		}

		if ( m_columns[MINUTE].index < 0 )
		{
			for( size_t i=0;i<nrecords;i++ )
				m_columns[MINUTE].data[i] = (float)( (step/2)/60 );
		}
	}

	

	
	// do the interpolation of meteorological data if requested in the header
	if ( interp || interpmet )
	{
		int met_indexes[] = { WSPD, WDIR, TDRY, TWET, TDEW, RH, PRES, SNOW, ALB, -1 };

		// apply to all met data relevant ids
		size_t j=0;
		while( met_indexes[j] >= 0 )
		{
			int idx = met_indexes[j]; // find column if it has been read in from the data file
			for( size_t i=0;i<nrecords;i++ )
			{
				if ( i==0 && nrecords > 1 )
				{
					// first time step: set to the backwards interpolation values of the first two time steps
					m_columns[idx].data[0] = m_columns[idx].data[1] 
						+ 1.5f*( m_columns[idx].data[0] - m_columns[idx].data[1] );
				}
				else
				{
					// set to the average of the current and previous
					m_columns[idx].data[i] = 0.5f*( m_columns[idx].data[i] 
						+ m_columns[idx].data[i-1] );
				}
			}
			j++;
		}
	}

	
	return true;
}

void weatherfile::rewind()
{
	if( !m_ok ) return;

	m_index = 0;
}


bool weatherfile::read()
{
	reset_record();
	if ( m_index >= 0 && m_index < nrecords )
	{
		year = (int)m_columns[YEAR].data[m_index];
		month = (int)m_columns[MONTH].data[m_index];
		day = (int)m_columns[DAY].data[m_index];
		hour = (int)m_columns[HOUR].data[m_index];
		minute = m_columns[MINUTE].data[m_index];
		gh = m_columns[GHI].data[m_index];
		dn = m_columns[DNI].data[m_index];
		df = m_columns[DHI].data[m_index];
		wspd = m_columns[WSPD].data[m_index];
		wdir = m_columns[WDIR].data[m_index];
		tdry = m_columns[TDRY].data[m_index];
		twet = m_columns[TWET].data[m_index];
		tdew = m_columns[TDEW].data[m_index];
		rhum = m_columns[RH].data[m_index];
		pres = m_columns[PRES].data[m_index];
		snow = m_columns[SNOW].data[m_index];
		albedo = m_columns[ALB].data[m_index];
		aod = m_columns[AOD].data[m_index];

		m_index++;
		return true;
	}
	else
		return false;
}




bool weatherfile::convert_to_wfcsv( const std::string &input, const std::string &output )
{
	weatherfile in( input );
	if ( !in.ok() ) return false;

	util::stdfile fp( output, "w" );
	if ( !fp.ok() ) return false;

	if ( in.type() == weatherfile::TMY2 )
	{
		fprintf(fp, "Source,Location ID,City,State,Country,Latitude,Longitude,Time Zone,Elevation\n");
		fprintf(fp, "TMY2,%s,%s,%s,USA,%.6lf,%.6lf,%lg,%lg\n", in.location.c_str(),
			normalize_city(in.city).c_str(), in.state.c_str(), in.lat, in.lon, in.tz, in.elev );
		fprintf(fp, "Year,Month,Day,Hour,GHI,DNI,DHI,Tdry,Tdew,RH,Pres,Wspd,Wdir,Snow Depth\n" );
		for( size_t i=0;i<8760;i++ )
		{
			if (!in.read()) return false;
			fprintf(fp, "%d,%d,%d,%d,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg\n",
				in.year, in.month, in.day, in.hour,
				in.gh, in.dn, in.df, in.tdry, in.tdew, in.rhum, in.pres, in.wspd, in.wdir, in.snow );
		}
	}
	else if ( in.type() == weatherfile::TMY3 )
	{
		fprintf(fp, "Source,Location ID,City,State,Country,Latitude,Longitude,Time Zone,Elevation\n");
		fprintf(fp, "TMY3,%s,%s,%s,USA,%.6lf,%.6lf,%lg,%lg\n", in.location.c_str(),
			normalize_city(in.city).c_str(), in.state.c_str(), in.lat, in.lon, in.tz, in.elev );
		fprintf(fp, "Year,Month,Day,Hour,GHI,DNI,DHI,Tdry,Twet,RH,Pres,Wspd,Wdir,Albedo\n" );
		for( size_t i=0;i<8760;i++ )
		{
			if (!in.read()) return false;
			fprintf(fp, "%d,%d,%d,%d,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg\n",
				in.year, in.month, in.day, in.hour,
				in.gh, in.dn, in.df, in.tdry, in.twet, in.rhum, in.pres, in.wspd, in.wdir, in.albedo );
		}
	}
	else if ( in.type() == weatherfile::EPW )
	{
		fprintf(fp, "Source,Location ID,City,State,Country,Latitude,Longitude,Time Zone,Elevation\n");
		fprintf(fp, "EPW,%s,%s,%s,%s,%.6lf,%.6lf,%lg,%lg\n", in.location.c_str(),
			normalize_city(in.city).c_str(), in.state.c_str(), in.country.c_str(), in.lat, in.lon, in.tz, in.elev );
		fprintf(fp, "Year,Month,Day,Hour,GHI,DNI,DHI,Tdry,Twet,RH,Pres,Wspd,Wdir,Albedo\n" );
		for( size_t i=0;i<8760;i++ )
		{
			if (!in.read()) return false;
			fprintf(fp, "%d,%d,%d,%d,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg\n",
				in.year, in.month, in.day, in.hour,
				in.gh, in.dn, in.df, in.tdry, in.twet, in.rhum, in.pres, in.wspd, in.wdir, in.albedo );
		}
	}
	else if ( in.type() == weatherfile::SMW )
	{
		fprintf(fp, "Source,Location ID,City,State,Latitude,Longitude,Time Zone,Elevation,Year\n");
		fprintf(fp, "SMW,%s,%s,%s,%.6lf,%.6lf,%lg,%lg,%d\n", in.location.c_str(),
			normalize_city(in.city).c_str(), in.state.c_str(), in.country.c_str(), in.lat, in.lon, in.tz, in.elev, in.year );
		fprintf(fp, "Month,Day,Hour,GHI,DNI,DHI,Tdry,Twet,Tdew,RH,Pres,Wspd,Wdir,Snow,Albedo\n" );
		for( size_t i=0;i<8760;i++ )
		{
			if (!in.read()) return false;
			fprintf(fp, "%d,%d,%d,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg,%lg\n",
				in.month, in.day, in.hour,
				in.gh, in.dn, in.df, in.tdry, in.twet, in.tdew, in.rhum, in.pres, in.wspd, in.wdir, in.snow, in.albedo );
		}
	}
	else
		return false;


	return true;

}
