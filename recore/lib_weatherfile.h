#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>


class weatherfile
{
private:
	FILE *m_fp;
	int m_type;
	std::string m_file;
	int m_startYear;
	double m_time;
	bool
		m_interp_mode,
		m_first_call;
	//Dynamic arrays for interpolation
	int* YEAR;
	int* MONTH;
	int* DAY;
	int* HOUR;
	double* MINUTE;
	double* GH;   /* global (Wh/m2) */
	double* DN;   /* direct (Wh/m2) */
	double* DF;   /* diffuse (Wh/m2) */
	double* WSPD; /* wind speed (m/s) */
	double* WDIR; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
	double* TDRY; /* dry bulb temp (C) */
	double* TWET; /* wet bulb temp (C) */
	double* TDEW; /* dew point temp (C) */
	double* RHUM; /* relative humidity (%) */
	double* PRES; /* pressure (mbar) */
	double* SNOW; /* snow depth (cm) 0-150 */
	double* ALBEDO; /* ground reflectance 0-1.  values outside this range mean it is not included */
	bool allocated;
	int ncall;


public:
	weatherfile();
	weatherfile( const std::string &file );
	~weatherfile();
	enum { INVALID, TMY2, TMY3, EPW, SMW };
	bool ok();
	int type();
	std::string filename();
	void close();
	bool open( const std::string &file );
	void rewind();
	void disable_interpolation();

	/******** header data *******/
	std::string loc_id;
	std::string city;
	std::string state;
	double tz;
	double lat;
	double lon;
	double elev;
	double start; // start time in seconds, 0 = jan 1st midnight
	double step; // step time in seconds
	int nrecords; // number of data records in file
	

	// reads one more record
	bool read();

	/******** record data ********/
	int year;
	int month;
	int day;
	int hour;
	double minute;
	double gh;   /* global (Wh/m2) */
	double dn;   /* direct (Wh/m2) */
	double df;   /* diffuse (Wh/m2) */
	double wspd; /* wind speed (m/s) */
	double wdir; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
	double tdry; /* dry bulb temp (C) */
	double twet; /* wet bulb temp (C) */
	double tdew; /* dew point temp (C) */
	double rhum; /* relative humidity (%) */
	double pres; /* pressure (mbar) */
	double snow; /* snow depth (cm) 0-150 */
	double albedo; /* ground reflectance 0-1.  values outside this range mean it is not included */
};

#endif

