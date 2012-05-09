#include <stdio.h>
#include <cmath>
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
#include "lib_windfile.h"


static int locate2(char *buf, char **colidx, int colmax, char delim)
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
char *gettoken2 (char *s, const char *delim, char **save_ptr)
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



windfile::windfile()
{
	m_fp = 0;
	close();
}

windfile::windfile( const std::string &file )
{
	m_fp = 0;
	close();
	open( file );
}

windfile::~windfile()
{
	if (m_fp != 0) fclose( m_fp );
}

bool windfile::ok()
{
	return m_fp != 0 && m_type != INVALID;
}

int windfile::type()
{
	return m_type;
}

std::string windfile::filename()
{
	return m_file;
}

bool windfile::open( const std::string &file )
{
	close();
	if (file.empty()) return false;
	
	if ( cmp_ext(file.c_str(), "swrf") )
		m_type = SWRF;
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
	if ( m_type==SWRF )
	{
		resource_ht = 0;
		//1Created 2010-09-28 10:07:36.991836 by http://scctest/wsis/wwis?lat=40.1222&lon=-102.725&year=2005
		//2Requested latitude = 40.1222
		//3Requested longitude = -102.725
		//4Latitude of data = 40.0917
		//5Longitude of data = -102.742
		//6Approximate distance from requested point = 3.8684 km
		//7Elevation = 1257.88 meters
		//8
		//9
		//10Date/Time (GMT)	Surface Pressure (mb)	Speed 200m (m/s)	Speed 100m (m/s)	Speed 80m (m/s)	Speed 50m (m/s)	Speed 10m (m/s)	Direction 200m (°)	Direction 100m (°)	Direction 80m (°)	Direction 50m (°)	Direction 10m (°)	Temperature 200m (°K)	Temperature 100m (°K)	Temperature 80m (°K)	Temperature 50m (°K)	Temperature 10m (°K)
		//11 - data starts

		char *lasts, *p;		
		lasts = NULL;
		fgets(buf, 2047, m_fp); // skip first line
		fgets(buf, 2047, m_fp); // skip requested latitude
		fgets(buf, 2047, m_fp); // skip requested longitude
		fgets(buf, 2047, m_fp); // latitude
		p = gettoken2( buf, "=", &lasts );  // label
		p = gettoken2( buf, "=", &lasts );  // value
		lat = atof( p );
		fgets(buf, 2047, m_fp); // longitude
		p = gettoken2( buf, "=", &lasts );  // label
		p = gettoken2( buf, "=", &lasts );  // value
		lon = atof( p );

		fgets(buf, 2047, m_fp); // skip distance from requested point
		fgets(buf, 2047, m_fp); // elevation
		p = gettoken2( buf, " ", &lasts );  // label
		p = gettoken2( buf, " ", &lasts );  // =
		p = gettoken2( buf, " ", &lasts );  // value
		elev = atof( p );

		fgets(buf, 2047, m_fp); // skip 8
		fgets(buf, 2047, m_fp); // skip 9
		fgets(buf, 2047, m_fp); // skip 10 (column labels)

		tz = 0; // GMT
		start = 1800;
		step = 3600;
		nrecords = 8760;
	}
	else
	{	
		close();
		return false;
	}
	
	return true;
}

void windfile::rewind()
{
	if (!ok()) return;
	
	::rewind( m_fp );
		
	// skip header info
	char buf[2048];
	if (m_type == SWRF)
		for(int i=0;i<10;i++) fgets(buf,2047,m_fp);
}

void windfile::close()
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


bool  windfile::read()
{
	char buf[1025];
	char *cols[128], *p;
	
	if ( !ok() ) return false;
	
	if ( m_type == SWRF )
	{
		// Date/Time (GMT)	Surface Pressure (mb)	Speed 200m (m/s)	Speed 100m (m/s)	Speed 50m (m/s)	Speed 20m (m/s)	Speed 10m (m/s)	Direction 200m (°)	Direction 100m (°)	Direction 50m (°)	Direction 20m (°)	Direction 10m (°)	Temperature 200m (°C)	Temperature 100m (°C)	Temperature 50m (°C)	Temperature 20m (°C)	Temperature 10m (°C)
		// 20050101 0100	0.88085523	18.57413101	18.04714012	17.18694305	15.59224415	14.17036247	214.22488403	214.22824097	214.33505249	214.55448914	214.54837036	6.55297852	7.20297241	7.52798462	7.81369019	7.88400269

		char *pret = fgets(buf, 1024, m_fp);
		int ncols = locate2(buf, cols, 128, '\t');

		if (ncols != 17) return false;
		
		int hr,min;
		p = cols[0];
		int nread = sscanf(p, "%4d%2d%2d %2d%2d", &year, &month, &day, &hr, &min);
		if (nread != 5) return false;
		hour = (hr == 0) ? 23 : hr-1; // hours in file go from 1 to 23, then 0; then 1, 2 ... 23, 0; etc. The first hour is labeled 1, not zero, although the hours go from 0 to 23

		pres = (double)atof(cols[1]);

		switch (resource_ht)
		{
			case 10:
				wspd = (double)atof(cols[6]);
				wdir = (double)atof(cols[11]);
				tdry = (double)atof(cols[16]);
				break;

			case 20:
				wspd = (double)atof(cols[5]);
				wdir = (double)atof(cols[10]);
				tdry = (double)atof(cols[15]);
				break;

			case 50:
				wspd = (double)atof(cols[4]);
				wdir = (double)atof(cols[9]);
				tdry = (double)atof(cols[14]);
				break;

			case 100:
				wspd = (double)atof(cols[3]);
				wdir = (double)atof(cols[8]);
				tdry = (double)atof(cols[13]);
				break;

			case 200:
				wspd = (double)atof(cols[2]);
				wdir = (double)atof(cols[7]);
				tdry = (double)atof(cols[12]);
				break;

			default: return false;
		}
		
		return pret==buf;
	}
	else
		return false;
}
