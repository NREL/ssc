#include <stdio.h>
#include <cmath>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <limits>
#include <numeric>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)
#define CASECMP(a,b) _stricmp(a,b)
#define CASENCMP(a,b,n) _strnicmp(a,b,n)
#else
#define CASECMP(a,b) strcasecmp(a,b)
#define CASENCMP(a,b,n) strncasecmp(a,b,n)
#endif

#include "lib_util.h"
#include "lib_windfile.h"

#ifdef _MSC_VER
#define my_isnan(x) ::_isnan( x )
#else
#define my_isnan(x) std::isnan( x )
#endif

static void trim( char *buf )
{
	if (!buf) return;

	size_t len = strlen(buf);
	if (len > 0 && buf[len-1] == '\n') // strip newline
		buf[len-1] = 0;

	if (len > 1 && buf[len-2] == '\r') // strip carriage return
		buf[len-2] = 0;
}

static int locate2(char *buf, char **colidx, int colmax, char delim)
{
	trim(buf);

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

/* 
   version of strtok_r from (2010/9/24)
   http://www.koders.com/c/fid9E7961E1E818E911DA7C34DD56DD8782726B3FCD.aspx
   */
static char *gettoken2 (char *s, const char *delim, char **save_ptr)
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

#define MBUFLEN 4096

windfile::windfile()
{
	m_buf = new char[MBUFLEN];
	m_fp = 0;
	close();
}

windfile::windfile( const std::string &file )
{
	m_buf = new char[MBUFLEN];
	m_fp = 0;
	close();
	open( file );
}

windfile::~windfile()
{
	delete [] m_buf;
	if (m_fp != 0) fclose( m_fp );
}

bool windfile::ok()
{
	return m_fp != 0;
}


std::string windfile::filename()
{
	return m_file;
}

bool windfile::open( const std::string &file )
{
	close();
	if (file.empty()) return false;
	
	/*  // don't be strict about requiring .wrf extensions for now
	if ( !cmp_ext(file.c_str(), "wrf") )
		return false;
		*/
		
	m_fp = fopen(file.c_str(), "r");
	if (!m_fp)
	{
		m_errorMsg = "could not open file for reading: " + file;
		return false;
	}
		
	/* read header information */
	

	// read line 1 (header info
	fgets( m_buf, MBUFLEN-1, m_fp );
	char *cols[128];
	int ncols = locate2(m_buf, cols, 128, ',');

	if (ncols != 10)
	{
		m_errorMsg = util::format("error reading header (line 1).  10 columns required, %d found.", ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}

	locid = std::string( cols[0] );
	city = std::string( cols[1] );
	state = std::string( cols[2] );
	country = std::string( cols[3] );

	year = atoi( cols[4] );
	lat = atof( cols[5] );
	lon = atof( cols[6] );
	elev = atof( cols[7] );

	
	// read line 2, description
	fgets( m_buf, MBUFLEN-1, m_fp );
	trim( m_buf );
	desc = std::string(m_buf);
	
	// read line 3, column names (must be pressure, temperature, speed, direction)
	fgets( m_buf, MBUFLEN-1, m_fp );
	ncols = locate2( m_buf, cols, 128, ',' );
	if (ncols < 3)
	{
		m_errorMsg = util::format("too few data column types found: %d.  at least 3 required.", ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}

	m_dataid.resize( ncols, INVAL );
	m_heights.resize( ncols, -1  );

	for (int i=0;i<ncols;i++)
	{
		std::string ctype = util::lower_case( cols[i] );
		if ( ctype == "temperature" || ctype == "temp" )
			m_dataid[i] = TEMP;
		else if ( ctype == "pressure" || ctype == "pres" )
			m_dataid[i] = PRES;
		else if ( ctype == "speed" || ctype == "velocity" )
			m_dataid[i] = SPEED;
		else if ( ctype == "direction" || ctype == "dir" )
			m_dataid[i] = DIR;
		else
		{
			m_errorMsg = util::format( "error reading data column type specifier in col %d of %d: '%s'", i+1, ncols, ctype.c_str() );
			fclose( m_fp );
			m_fp = 0;
			return false;
		}
	}


	// read line 4, units for each column (ignore this for now)
	fgets( m_buf, MBUFLEN-1, m_fp );

	// read line 5, height in meters for each data column
	fgets( m_buf, MBUFLEN-1, m_fp );
	ncols = locate2( m_buf, cols, 128, ',' );
	if ( ncols < (int)m_heights.size() )
	{
		m_errorMsg = util::format("too few columns in the height row.  %d required but only %d found", (int)m_heights.size(), ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}

	for (int i=0;i<ncols;i++)
		m_heights[i] = atof( cols[i] );
	
	// ready to read line-by-line.  subsequent columns of data correspond to the
	// data types in m_dataid and measurement heights in m_heights

	m_file = file;
	return true;
}

void windfile::close()
{
	if ( m_fp != 0 ) fclose( m_fp );
	
	m_fp = 0;
	m_file.clear();
	city.clear();
	state.clear();
	locid.clear();
	country.clear();
	desc.clear();
	year = 1900;
	lat = lon = elev = 0.0;
}


int windfile::find_closest( int id, double requested_height, double *meas_height_found )
{
	int closest_index = -1;
	double height_diff = 1e99, meas_height = 0;
	for ( size_t i=0;i<m_dataid.size();i++ )
	{
		if ( m_dataid[i] == id )
		{
			if ( fabs(m_heights[i] - requested_height) < height_diff )
			{
				closest_index = i;
				height_diff = fabs(m_heights[i] - requested_height);
				meas_height = m_heights[i];
			}
		}
	}

	if ( meas_height_found != 0 )
		*meas_height_found = meas_height;

	return closest_index;
}

std::vector<double> windfile::read()
{
	char *cols[128];	
	std::vector<double> values;
	fgets( m_buf, MBUFLEN-1, m_fp );
	int ncols = locate2( m_buf, cols, 128, ',' );
	
	if (ncols == m_heights.size() 
		&& ncols == m_dataid.size())
	{
		values.resize( ncols, 0.0 );
		for (int i=0;i<ncols;i++)
			values[i] = atof( cols[i] );
	}

	return values;
}

bool windfile::read( double requested_height,
	double *speed,
	double *direction,
	double *temperature,
	double *pressure,
	double *closest_speed_meas_height_in_file,
	double *closest_dir_meas_height_in_file )
{
	char *cols[128];	
	double values[128];
	if ( !ok() ) return false;
	
	fgets( m_buf, MBUFLEN-1, m_fp );
	int ncols = locate2( m_buf, cols, 128, ',' );

	if (ncols < (int)m_heights.size() || ncols < (int)m_dataid.size())
		return false;

	for ( int i=0;i<ncols;i++ )
		values[i] = atof( cols[i] );

	*speed = *direction = *temperature = *pressure = *closest_speed_meas_height_in_file = *closest_dir_meas_height_in_file = std::numeric_limits<double>::quiet_NaN();

	double speed_meas_ht_found;
	int index = find_closest( SPEED, requested_height, &speed_meas_ht_found );
	if (index >= 0 && index < ncols)
		*speed = values[index];

	double dir_meas_ht_found;
	index = find_closest( DIR, requested_height, &dir_meas_ht_found );
	if (index >= 0 && index < ncols)
		*direction = values[index];

	index = find_closest( TEMP, requested_height, 0 );
	if (index >= 0 && index < ncols)
		*temperature = values[index];

	index = find_closest( PRES, requested_height, 0 );
	if (index >= 0 && index < ncols)
		*pressure = values[index];

	bool found_all 
		= !my_isnan( *speed )
		&& !my_isnan( *direction )
		&& !my_isnan( *temperature )
		&& !my_isnan( *pressure );

	if (found_all)
	{
		*closest_speed_meas_height_in_file = speed_meas_ht_found;
		*closest_dir_meas_height_in_file = dir_meas_ht_found;
	}
	return found_all;

}
