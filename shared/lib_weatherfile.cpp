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


static int locate(char *buf, char **colidx, int colmax, char delim)
{
	char *p = buf;
	int i = 1;
	int ncols = 0;
	
	colidx[0] = p;
	while (p && *p && i < colmax)
	{
		p = strchr(p, delim);
		if (p) colidx[i++] = ++p;
	}

	ncols = i;

	while (i<colmax) colidx[i++] = 0;

	return ncols;
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



weatherfile::weatherfile()
{
	m_interp_mode = true;
	m_first_call = true;
	ncall = 0;
	allocated = false;
	m_fp = 0;
	close();
}

weatherfile::weatherfile( const std::string &file )
{
	m_interp_mode = false;
	m_first_call = true;
	ncall = 0;
	allocated = false;
	m_fp = 0;
	close();
	open( file );
}

weatherfile::~weatherfile()
{
	if (m_fp != 0) fclose( m_fp );

	//Delete dynamic arrays
	if(allocated){
		delete [] YEAR;
		delete [] MONTH;
		delete [] DAY;
		delete [] HOUR;
		delete [] MINUTE;
		delete [] GH;   /* global (Wh/m2) */
		delete [] DN;   /* direct (Wh/m2) */
		delete [] DF;   /* diffuse (Wh/m2) */
		delete [] WSPD; /* wind speed (m/s) */
		delete [] WDIR; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
		delete [] TDRY; /* dry bulb temp (C) */
		delete [] TWET; /* wet bulb temp (C) */
		delete [] TDEW; /* dew point temp (C) */
		delete [] RHUM; /* relative humidity (%) */
		delete [] PRES; /* pressure (mbar) */
		delete [] SNOW; /* snow depth (cm) 0-150 */
		delete [] ALBEDO; /* ground reflectance 0-1.  values outside this range mean it is not included */
	}

}

bool weatherfile::ok()
{
	return m_fp != 0 && m_type != INVALID;
}

int weatherfile::type()
{
	return m_type;
}

std::string weatherfile::filename()
{
	return m_file;
}

bool weatherfile::open( const std::string &file )
{
	close();
	if (file.empty()) return false;
	
	if (cmp_ext(file.c_str(),"tm2") || cmp_ext(file.c_str(), "tmy2"))
		m_type = TMY2;
	else if ( cmp_ext(file.c_str(), "tm3") || cmp_ext(file.c_str(), "tmy3") || cmp_ext(file.c_str(), "csv") )
		m_type = TMY3;
	else if ( cmp_ext(file.c_str(), "epw") )
		m_type = EPW;
	else if ( cmp_ext(file.c_str(), "smw") )
		m_type = SMW;
	else
		return false;
	
	char buf[2048];
	
	
	m_fp = fopen(file.c_str(), "r");
	if (!m_fp)
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
		
		fgets(buf, 2047, m_fp);
		sscanf(buf, "%s %s %s %lg %s %d %d %s %d %d %d",
				pl, pc, ps,
				&tz,
				slat,&dlat,&mlat,
				slon,&dlon,&mlon,
				&ielv);
		
		lat = conv_deg_min_sec(dlat, mlat, 0, slat[0]);
		lon = conv_deg_min_sec(dlon, mlon, 0, slon[0]);
		loc_id = pl;
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
		
		fgets(buf, 2047, m_fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		loc_id = p!=0 ? p : "";
				
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

		fgets(buf, 2047, m_fp); /* skip over labels line */
	}
	else if( m_type==EPW )
	{
	/*  LOCATION,Cairo Intl Airport,Al Qahirah,EGY,ETMY,623660,30.13,31.40,2.0,74.0 */
	/*  LOCATION,Alice Springs Airport,NT,AUS,RMY,943260,-23.80,133.88,9.5,547.0 */
		char *lasts, *p;
		
		fgets(buf, 2047, m_fp);
		
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
		loc_id = p!=0 ? p : "";
		
		p = gettoken( NULL, ",", &lasts );
		lat = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		lon = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		tz = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		elev = atof(p);
		
		/* skip over excess header lines */
		
		fgets(buf,2047,m_fp); /* DESIGN CONDITIONS */
		fgets(buf,2047,m_fp); /* TYPICAL/EXTREME PERIODS */
		fgets(buf,2047,m_fp); /* GROUND TEMPERATURES */
		fgets(buf,2047,m_fp); /* HOLIDAY/DAYLIGHT SAVINGS */
		fgets(buf,2047,m_fp); /* COMMENTS 1 */
		fgets(buf,2047,m_fp); /* COMMENTS 2 */
		fgets(buf,2047,m_fp); /* DATA PERIODS */
		
		start = 1800;
		step = 3600;
		nrecords = 8760;
		
	}
	else if ( m_type==SMW )
	{
		char *lasts, *p;
		
		fgets(buf, 2047, m_fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		loc_id = p!=0 ? p : "";

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
		while (fgets(buf, 2047, m_fp ) != 0)
			nrecords ++;

		::rewind( m_fp );
		fgets( buf, 2047, m_fp );
	}
	else
	{	
		close();
		return false;
	}
	
	return true;
}

void weatherfile::rewind()
{
	if (!ok()) return;
	
	::rewind( m_fp );
		
	// skip header info
	char buf[2048];
	if (m_type == TMY2 || m_type == SMW)
		fgets(buf,2047, m_fp); 
	else if(m_type==TMY3)
		for(int i=0;i<2;i++) fgets(buf,2047,m_fp);
	else if(m_type==EPW)
		for(int i=0;i<8;i++) fgets(buf,2047,m_fp);
}

void weatherfile::close()
{
	if ( m_fp != 0 ) fclose( m_fp );
	
	m_fp = 0;
	m_type = INVALID;
	m_file.clear();
	m_startYear = 1900;
	m_time = 0.0;

	loc_id.clear();
	city.clear();
	state.clear();
	tz = lat = lon = elev = 0.0;
	start = step = 0.0;
	nrecords = 0;

	year = month = day = hour = 0;
	minute = 0.0;
	gh = dn = df = wspd = wdir = 0.0;
	tdry = twet = tdew = rhum = pres = snow = albedo = 0.0;

}

void weatherfile::disable_interpolation()
{
	m_interp_mode = false;
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

bool  weatherfile::read()
{
	char buf[1025];
	char *cols[128], *p;
	
	if ( !ok() ) return false;
	
	if ( m_type == TMY2 )
	{
		int yr,mn,dy,hr,ethor,etdn;
		int d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21;      /* which of these are used? d3, d10, d15 & d20 */
		int u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21;  /* are any of these ever used?? */
		int w1,w2,w3,w4,w5,w6,w7,w8,w9,w10;     
		char f1[2],f2[2],f3[2],f4[2],f5[2],f6[2],f7[2],f8[2],f9[2],f10[2],f11[2],f12[2],f13[2],f14[2],f15[2],f16[2],f17[2],f18[2],f19[2],f20[2],f21[2];
		
		if(m_first_call || !m_interp_mode){

			if(! allocated && m_interp_mode){
				YEAR = new int[nrecords];
				MONTH = new int[nrecords];
				DAY = new int[nrecords];
				HOUR = new int[nrecords];
				MINUTE = new double[nrecords];
				GH = new double[nrecords];   /* global (Wh/m2) */
				DN = new double[nrecords];   /* direct (Wh/m2) */
				DF = new double[nrecords];   /* diffuse (Wh/m2) */
				WSPD = new double[nrecords]; /* wind speed (m/s) */
				WDIR = new double[nrecords]; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
				TDRY = new double[nrecords]; /* dry bulb temp (C) */
				TWET = new double[nrecords]; /* wet bulb temp (C) */
				TDEW = new double[nrecords]; /* dew point temp (C) */
				RHUM = new double[nrecords]; /* relative humidity (%) */
				PRES = new double[nrecords]; /* pressure (mbar) */
				SNOW = new double[nrecords]; /* snow depth (cm) 0-150 */
				ALBEDO = new double[nrecords]; /* ground reflectance 0-1.  values outside this range mean it is not included */
				allocated = true;
			}

			for(int i=0; i<nrecords; i++){		//Read in each record
				

				char *pret = fgets(buf, 1023, m_fp);
		
	
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

				year = yr + 1900;
				month = mn;
				day = dy;
				hour = hr-1;  // hour goes 0-23, not 1-24
				minute = 30;
				gh = (double)d1*1.0;
				dn=(double)d2;           /* Direct radiation */
				df=(double)d3;           /* Diffuse radiation */
				tdry=(double)d10/10.0;       /* Ambient dry bulb temperature(C) */
				tdew = (double)d11/10.0; /* dew point temp */
				wspd=(double)d15/10.0;       /* Wind speed(m/s) */
				wdir=(double)d14; /* wind dir */
				rhum = (double)d12;
				pres = (double)d13;
				snow = (double)d20;
				albedo = -999; /* no albedo in TMY2 */
						
				twet = calc_twet( tdry, rhum, pres ); /* must calculate wet bulb */

				if(allocated && m_interp_mode){
					HOUR[i] = hour;
					MINUTE[i] = minute;
					DAY[i] = day;
					MONTH[i] = month;
					YEAR[i] = year;
					GH[i] = gh;
					DN[i] = dn;
					DF[i] = df;
					WSPD[i] = wspd;
					WDIR[i] = wdir;
					TDRY[i] = tdry;
					TWET[i] = twet;
					TDEW[i] = tdew;
					RHUM[i] = rhum;
					PRES[i] = pres;
					SNOW[i] = snow;
					ALBEDO[i] = albedo;
				}

				if(!(nread==79 && pret==buf)) return false;
				if(! m_interp_mode) break;	//Don't read multiple records if we're not in interp mode
			}
		}
		//if this is the first call, set to the backwards interpolation values of the first two time steps
		if(m_interp_mode){
				
			hour = HOUR[ncall];
			minute = MINUTE[ncall];
			gh = GH[ncall];
			dn = DN[ncall];
			df = DF[ncall];
			month = MONTH[ncall];
			day = DAY[ncall];

			if(m_first_call){
				wspd = WSPD[1] + 1.5*(WSPD[0] - WSPD[1]);
				wdir = WDIR[1] + 1.5*(WDIR[0] - WDIR[1]);
				tdry = TDRY[1] + 1.5*(TDRY[0] - TDRY[1]);
				twet = TWET[1] + 1.5*(TWET[0] - TWET[1]);
				tdew = TDEW[1] + 1.5*(TDEW[0] - TDEW[1]);
				rhum = RHUM[1] + 1.5*(RHUM[0] - RHUM[1]);
				pres = PRES[1] + 1.5*(PRES[0] - PRES[1]);
				snow = SNOW[1] + 1.5*(SNOW[0] - SNOW[1]);
				albedo = ALBEDO[1] + 1.5*(ALBEDO[0] - ALBEDO[1]);
				m_first_call = false;
			}
			else{
				wspd = 0.5*(WSPD[ncall] +WSPD[ncall-1]);
				wdir = 0.5*(WDIR[ncall] +WDIR[ncall-1]);
				tdry = 0.5*(TDRY[ncall] +TDRY[ncall-1]);
				twet = 0.5*(TWET[ncall] +TWET[ncall-1]);
				tdew = 0.5*(TDEW[ncall] +TDEW[ncall-1]);
				rhum = 0.5*(RHUM[ncall] +RHUM[ncall-1]);
				pres = 0.5*(PRES[ncall] +PRES[ncall-1]);
				snow = 0.5*(SNOW[ncall] +SNOW[ncall-1]);
				albedo = 0.5*(ALBEDO[ncall] +ALBEDO[ncall-1]);

			}
			ncall++;
		}

		return true;
	}
	else if ( m_type == TMY3 )
	{
		char *pret = fgets(buf, 1023, m_fp);
		int ncols = locate(buf, cols, 128, ',');

		if (ncols < 68)
			return false;
	
		p = cols[0];

		month = atoi( p );
		p = strchr(p, '/');
		if (!p)
			return false;
		p++;
		day = atoi( p );
		p = strchr(p, '/');
		if (!p) return false;
		p++;
		year = atoi( p );

		hour = atoi( cols[1] ) - 1;  // hour goes 0-23, not 1-24
		minute = 30;

		gh = (double)atof( cols[4] );
		dn = (double)atof( cols[7] );
		df = (double)atof( cols[10] );

		tdry = (double)atof( cols[31] );
		twet = (double)atof( cols[34] );

		wspd = (double)atof( cols[46] );
		wdir = (double)atof( cols[43] );

		rhum = (double)atof( cols[37] );
		pres = (double)atof( cols[40] );
		snow = 999.0; // no snowfall in TMY3
		albedo = (double)atof( cols[61] );
				
		tdew = wiki_dew_calc( tdry, rhum );

		return pret==buf;
	}
	else if ( m_type == EPW)
	{
		char *pret = fgets(buf, 1024, m_fp);
		int ncols = locate(buf, cols, 128, ',');

		if (ncols < 32)
			return false;

		year = atoi(cols[0]);
		month = atoi(cols[1]);
		day = atoi(cols[2]);
		hour = atoi(cols[3])-1;  // hour goes 0-23, not 1-24;
		minute = 30;

		gh = (double)atof(cols[13]);
		dn = (double)atof(cols[14]);
		df = (double)atof(cols[15]);
		
		wspd = (double)atof(cols[21]);
		wdir = (double)atof(cols[20]);
		
		tdry = (double)atof(cols[6]);
		twet = (double)atof(cols[7]);
		
		rhum = (double)atof( cols[8] );
		pres = (double)atof( cols[9] ) * 0.01; /* convert Pa in to mbar */
		snow = (double)atof( cols[30] ); // snowfall
		albedo = -999; /* no albedo in EPW file */

		tdew = wiki_dew_calc( tdry, rhum );

		return pret==buf;
	}
	else if ( m_type == SMW )
	{
		char *pret = fgets(buf, 1024, m_fp);
		int ncols = locate(buf, cols, 128, ',');

		if (ncols < 12)
			return false;
		
		double T = m_time;
		
		year = m_startYear; // start year
		month = util::month_of( T/3600.0 ); // 1-12
		day = util::day_of_month( month, T/3600.0 ); // 1-nday
		hour = ((int)(T/3600.0))%24;  // hour goes 0-23, not 1-24;
		minute = fmod(T/60.0, 60.0);      // minute goes 0-59
					

		m_time += step; // increment by step

		gh = (double)atof(cols[7]);
		dn = (double)atof(cols[8]);
		df = (double)atof(cols[9]);
		
		wspd = (double)atof(cols[4]);
		wdir = (double)atof(cols[5]);
		
		tdry = (double)atof(cols[0]);
		tdew = (double)atof(cols[1]);
		twet = (double)atof(cols[2]);
		
		rhum = (double)atof( cols[3] );
		pres = (double)atof( cols[6] );
		snow = (double)atof( cols[11] );
		albedo = (double)atof( cols[10] );
		
		return pret==buf;
	}
	else
		return false;
}







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

static int locate2(char *buf, char **colidx, int colmax, char delim)
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


wfcsv::wfcsv()
{
	reset();
}

wfcsv::wfcsv( const std::string &file )
{
	m_errorCode = read_all( file );
}

wfcsv::~wfcsv()
{
	if( m_fp != 0 )
		fclose( m_fp );
}

void wfcsv::reset()
{
	m_fp = 0;
	m_errorCode = -1;
	m_numRecords = 0;
	m_timeStepSeconds = 3600;
	m_columns.clear();
	m_hdrInterpMet = m_hdrHasUnits = false;
	m_hdrLocId = m_hdrCity = m_hdrState = m_hdrCountry 
		= m_hdrSource = m_hdrDescription = m_hdrURL = "";
	m_hdrLatitude = m_hdrLongitude = m_hdrTimeZone = m_hdrElevation = std::numeric_limits<double>::quiet_NaN();
	m_hdrYear = 0;
}

bool wfcsv::ok()
{
	return m_errorCode == 0;
}

std::string wfcsv::normalize_city( const std::string &in )
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

#define NBUF 2048
#define NCOL 128



int wfcsv::read_header( const std::string &file, bool leave_open )
{
	reset();

	util::stdfile fp( file, "r" );
	if ( !fp.ok() ) return -2;

	char buf[NBUF+1], *pbuf, 
		buf1[NBUF+1], *pbuf1, 
		*cols[128], *cols1[128];

	pbuf = fgets( buf, NBUF, fp );
	int ncols = locate2( buf, cols, NCOL, ',' );
	pbuf1 = fgets( buf1, NBUF, fp );
	int ncols1 = locate2( buf1, cols1, NCOL, ',' );

	if ( ncols != ncols1 
		|| pbuf != buf 
		|| pbuf1 != buf1 ) return -3; // first two lines must have same number of items

	for ( size_t i=0;i<ncols; i++ )
	{
		std::string name = util::lower_case( trimboth( cols[i] ) );
		char *value = trimboth(cols1[i]);

		if ( name == "lat" || name == "latitude" )
		{
			m_hdrLatitude = atof( value );
		}
		else if ( name == "lon" || name == "long" || name == "longitude" || name == "lng" )
		{
			m_hdrLongitude = atof( value );
		}
		else if ( name == "tz" || name == "timezone" || name == "time zone" )
		{
			m_hdrTimeZone = atof( value );
		}
		else if ( name == "el" || name == "elev" || name == "elevation" || name == "site elevation" )
		{
			m_hdrElevation = atof( value );
		}
		else if ( name == "year" )
		{
			m_hdrYear = atoi( value );
		}
		else if ( name == "id" || name == "location" || name == "location id" || name == "station" || name == "station id" || name == "wban" || name == "wban#" )
		{
			m_hdrLocId = value;
		}
		else if ( name == "city" )
		{
			m_hdrCity = value;
		}
		else if ( name == "state" || name == "province" || name == "region" )
		{
			m_hdrState = value;
		}
		else if ( name == "country" )
		{
			m_hdrCountry = value;
		}
		else if ( name == "source" || name == "src" )
		{
			m_hdrSource = value;
		}
		else if ( name == "description" || name == "desc" )
		{
			m_hdrDescription = value;
		}
		else if ( name == "url" )
		{
			m_hdrURL = value;
		}
		else if ( name == "hasunits" || name == "units" )
		{
			m_hdrHasUnits = ( util::lower_case( value ) == "yes"  || atoi( value ) != 0 );
		}
		else if ( name == "interpmet" )
		{
			m_hdrInterpMet = ( util::lower_case( value ) == "yes" || atoi( value ) != 0 );
		}
	}

	if ( leave_open )
		m_fp = fp.disown();

	return 0;
}

int wfcsv::read_data()
{
	util::stdfile fp( m_fp );
	
	m_fp = 0; // clear stored file pointer, reserved in stdfile above which took ownership of the pointer

	if ( !fp.ok() ) return -99; // file is not open, exit

	char buf[NBUF+1], *pbuf, 
		buf1[NBUF+1], *pbuf1, 
		*cols[128], *cols1[128];

	pbuf = fgets( buf, NBUF, fp ); // read column names	
	if ( pbuf != buf ) return -4;
	int ncols = locate2( buf, cols, NCOL, ',' );

	if ( m_hdrHasUnits )
	{
		pbuf1 = fgets( buf1, NBUF, fp ); // read column units;
		if ( pbuf1 != buf1 ) return -5;
		int ncols1 = locate2( buf1, cols1, NCOL, ',' );

		if ( ncols != ncols1 ) return -6;
	}

	// determine columns
	for( size_t i=0;i<ncols;i++ )
	{
		char *name_cstr = trimboth( cols[i] );
		if ( name_cstr && strlen(name_cstr) > 0 )
		{
			m_columns.push_back( column() );
			column &cc = m_columns[ m_columns.size()-1 ];
			cc.name = name_cstr;
			if ( m_hdrHasUnits )
				cc.units = trimboth( cols[1] );

			cc.index = i;

			std::string lowname = util::lower_case( cc.name );
			
			if ( lowname == "yr" || lowname == "year" ) cc.id = YEAR;
			else if ( lowname == "mo" || lowname == "month" ) cc.id = MONTH;
			else if ( lowname == "day" ) cc.id = DAY;
			else if ( lowname == "hour" || lowname == "hr" ) cc.id = HOUR;
			else if ( lowname == "min" || lowname == "minute" ) cc.id = MINUTE;
			else if ( lowname == "ghi" || lowname == "gh" || lowname == "global" || lowname == "global horizontal" || lowname == "global horizontal irradiance" ) cc.id = GHI;
			else if ( lowname == "dni" || lowname == "dn" || lowname == "beam" || lowname == "direct normal" || lowname == "direct normal irradiance" ) cc.id = DNI;
			else if ( lowname == "dhi" || lowname == "df" || lowname == "diffuse" || lowname == "diffuse horizontal" || lowname == "diffuse horizontal irradiance" ) cc.id = DHI;
			else if ( lowname == "tdry" || lowname == "dry bulb" || lowname == "dry bulb temp" || lowname == "temperature" || lowname == "ambient" || lowname == "ambient temp" ) cc.id = TDRY;
			else if ( lowname == "twet" || lowname == "wet bulb" || lowname == "wet bulb temperature" ) cc.id = TWET;
			else if ( lowname == "tdew" || lowname == "dew point" || lowname == "dew point temperature" ) cc.id = TDEW;
			else if ( lowname == "wspd" || lowname == "wind speed" ) cc.id = WSPD;
			else if ( lowname == "wdir" || lowname == "wind direction" ) cc.id = WDIR;
			else if ( lowname == "rh" || lowname == "rhum" && lowname == "relative humidity" || lowname == "humidity" ) cc.id = RH;
			else if ( lowname == "pres" || lowname == "pressure" ) cc.id = PRES;
			else if ( lowname == "snow" || lowname == "snow cover" || lowname == "snow depth" ) cc.id = SNOW;
			else if ( lowname == "alb" || lowname == "albedo" ) cc.id = ALB;
			else if ( lowname == "aod" || lowname == "aerosol" || lowname == "aerosol optical depth" ) cc.id = AOD;
		}
	}

	// preallocate 8760 values for each column - most common.
	for ( size_t i=0;i<ncols;i++ )
		m_columns[i].data.reserve( 8760 );
	
	m_numRecords = 0;
	// read data lines
	while( ! ::feof( fp ) )
	{
		buf[0] = 0;
		fgets( buf, NBUF, fp );
		pbuf = trimboth(buf);
		if ( !pbuf || !*pbuf ) break;

		ncols = locate2( pbuf, cols, NCOL, ',' );		
		if ( ncols < m_columns.size() ) return -7;

		m_numRecords++;
		for ( size_t i=0;i<m_columns.size(); i++ )
		{
			std::vector<float> &arr = m_columns[i].data;
			if ( arr.capacity() < m_numRecords )
				arr.reserve( arr.capacity() + 1000 );

			if ( i < ncols ) arr.push_back( (float)atof( trimboth(cols[i]) ) );
			else arr.push_back( std::numeric_limits<float>::quiet_NaN() );
		}
	}
	

	// all the data has been read in now
	fp.close(); // don't keep the file open any longer than we need to

	// do the interpolation of meteorological data if requested in the header
	if ( m_hdrInterpMet )
	{
		int met_indexes[] = { WSPD, WDIR, TDRY, TWET, TDEW, RH, PRES, SNOW, ALB, -1 };

		// apply to all met data relevant ids
		size_t j=0;
		while( met_indexes[j] >= 0 )
		{
			int idx = colindex( met_indexes[j] ); // find column if it has been read in from the data file
			if ( idx >= 0 )
			{
				for( size_t i=0;i<m_numRecords;i++ )
				{
					if ( i==0 && m_numRecords > 1 )
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
			}

			j++;
		}
	}

	// determine timestep as best as possible
	int nmult = m_numRecords / 8760;
	int minidx = colindex( MINUTE );
	int hridx = colindex( HOUR );
	if ( nmult*8760 == m_numRecords )
	{
		// multiple of 8760 records: assume 1 year of data
		m_timeStepSeconds = 3600/nmult;
	}
	else if ( minidx >= 0 && hridx >= 0 && m_numRecords > 1 )
	{
		// unrecognized length: take time difference between first two records
		float min0 = m_columns[minidx].data[0];
		float min1 = m_columns[minidx].data[1];
		float hr0 = m_columns[hridx].data[0];
		float hr1 = m_columns[hridx].data[1];

		int sec0 = (int)( hr0*3600 + min0*60 );
		int sec1 = (int)( hr1*3600 + min1*60 );

		m_timeStepSeconds = sec1 - sec0;
	}
	else
		return -8;

	return 0;
}

int wfcsv::read_all( const std::string &file )
{
	int code = read_header( file, true );
	if ( code < 0 )
		return code;

	return read_data();
}


int wfcsv::time_step_seconds()
{
	return m_timeStepSeconds;
}

float wfcsv::time_step_hours()
{
	return m_timeStepSeconds / 3600.0f;
}

size_t wfcsv::num_records()
{
	return m_numRecords;
}

bool wfcsv::has_data( int id )
{
	return colindex(id) >= 0;
}


int wfcsv::colindex( int id )
{
	for ( size_t i=0;i<m_columns.size();i++ )
		if ( m_columns[i].id == id )
			return (int) i;

	return -1;
}

float wfcsv::value( int id, size_t index )
{
	if ( index >= m_numRecords ) return std::numeric_limits<float>::quiet_NaN();

	int col = colindex( id );
	if ( col >= 0 )
	{
		return m_columns[col].data[index];
	}
	else
	{
		// special handling for certain columns that we can calculate from others
		// if the data doesn't exist
		if ( id == TWET )
		{
			int iT = colindex( TDRY );
			int iP = colindex( PRES );
			int iR = colindex( RH );
			if ( iT >= 0 && iP >= 0 && iR >= 0 )
				return (float)calc_twet( m_columns[iT].data[index],
					m_columns[iR].data[index], 
					m_columns[iP].data[index] );
		}
		else if ( id == TDEW )
		{
			int iT = colindex( TDRY );
			int iR = colindex( RH );
			if ( iT >= 0 && iR >= 0 )
				return (float) wiki_dew_calc( m_columns[iT].data[index], m_columns[iR].data[index] );
		}
		else if ( id == YEAR && m_hdrYear > 0 ) 
		{
			return (float)m_hdrYear;
		}
		else if ( id == MONTH )
		{
			if ( m_timeStepSeconds == 3600 && m_numRecords == 8760 )
				return util::month_of( index );
		}
		else if ( id == DAY )
		{
			if ( m_timeStepSeconds == 3600 && m_numRecords == 8760 )
			{
				int month = util::month_of( index );
				return util::day_of_month( month, index );
			}
		}
		else if ( id == HOUR )
		{
			if ( m_timeStepSeconds == 3600 && m_numRecords == 8760 )
			{
				int day = index / 24;
				int start_of_day = day*24;
				return index - start_of_day;
			}
		}
		else if ( id == MINUTE && m_timeStepSeconds > 0 )
		{
			return (m_timeStepSeconds/2)/60;
		}
	}

	// if all else fails, return NaN: data not 
	// supplied and could not be reasonably guessed
	return std::numeric_limits<float>::quiet_NaN();
}

std::vector<int> wfcsv::get_columns()
{
	std::vector<int> ids;
	for( size_t i=0;i<m_columns.size();i++ )
		ids.push_back( m_columns[i].id );
	return ids;
}

std::string wfcsv::get_canonical_name( int id )
{
static const char *canonical_names[_MAXCOL_] = {
		"year", "month", "day", "hour", "minute",
		"gh", "dn", "df",
		"tdry", "twet", "tdew",
		"wspd", "wdir", "rh",
		"pres", "snow", "alb", "aod" };

	if ( id >= 0 && id < _MAXCOL_ ) return canonical_names[id];
	else return "";
}


bool wfcsv::convert( const std::string &input, const std::string &output )
{
	weatherfile in( input );
	if ( !in.ok() ) return false;

	util::stdfile fp( output, "w" );
	if ( !fp.ok() ) return false;

	if ( in.type() == weatherfile::TMY2 )
	{
		fprintf(fp, "Source,Location ID,City,State,Country,Latitude,Longitude,Time Zone,Elevation\n");
		fprintf(fp, "TMY2,%s,%s,%s,USA,%.6lf,%.6lf,%lg,%lg\n", in.loc_id.c_str(),
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
		fprintf(fp, "TMY3,%s,%s,%s,USA,%.6lf,%.6lf,%lg,%lg\n", in.loc_id.c_str(),
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
		fprintf(fp, "EPW,%s,%s,%s,%s,%.6lf,%.6lf,%lg,%lg\n", in.loc_id.c_str(),
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
		fprintf(fp, "SMW,%s,%s,%s,%.6lf,%.6lf,%lg,%lg,%d\n", in.loc_id.c_str(),
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
