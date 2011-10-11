#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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
	m_fp = 0;
	close();
}

weatherfile::weatherfile( const std::string &file )
{
	m_fp = 0;
	close();
	open( file );
}

weatherfile::~weatherfile()
{
	if (m_fp != 0) fclose( m_fp );
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
		/* skip COUNTRY */
		
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
	tdry = twet = rhum = pres = snow = albedo = 0.0;

}


bool  weatherfile::read()
{
	char buf[1025];
	char *cols[128], *p;
	
	if ( !ok() ) return false;
	
	if ( m_type == TMY2 )
	{
		char *pret = fgets(buf, 1023, m_fp);
		
		int yr,mn,dy,hr,ethor,etdn;
		int d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21;      /* which of these are used? d3, d10, d15 & d20 */
		int u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21;  /* are any of these ever used?? */
		int w1,w2,w3,w4,w5,w6,w7,w8,w9,w10;     
		char f1[2],f2[2],f3[2],f4[2],f5[2],f6[2],f7[2],f8[2],f9[2],f10[2],f11[2],f12[2],f13[2],f14[2],f15[2],f16[2],f17[2],f18[2],f19[2],f20[2],f21[2];
	
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
		twet = (double)d11/10.0;
		wspd=(double)d15/10.0;       /* Wind speed(m/s) */
		wdir=(double)d14; /* wind dir */
		rhum = (double)d12;
		pres = (double)d13;
		snow = (double)d20;
		albedo = -999; /* no albedo in TMY2 */

		return nread==79 && pret==buf;
	}
	else if ( m_type == TMY3)
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
