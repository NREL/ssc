#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>
#include <vector>  // needed to compile in typelib_vc2012


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
	std::string country;
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


class wfcsv
{
private:

	struct column
	{
		std::string name;
		std::string units;
		size_t index;
		int id;
		std::vector<float> data;
	};

	int m_errorCode;
	size_t m_numRecords;
	int m_timeStepSeconds;
	std::vector<column> m_columns;
	
	// possible header data fields
	bool m_hdrInterpMet, m_hdrHasUnits;
	std::string m_hdrLocId, m_hdrCity, m_hdrState, m_hdrCountry,
		m_hdrSource, m_hdrDescription, m_hdrURL;
	double m_hdrLatitude, m_hdrLongitude, m_hdrTimeZone, m_hdrElevation;
	int m_hdrYear;

	void reset();
	int colindex( int id );

public:
	wfcsv();
	wfcsv( const std::string &file );

	bool ok();

	// return 0 on success, or negative error code
	int read_all( const std::string &file ); 
		
	enum { YEAR, MONTH, DAY, HOUR, MINUTE,
		GHI, DNI, DHI, 
		TDRY, TWET, TDEW, 
		WSPD, WDIR, 
		RH, PRES, SNOW, ALB, AOD,
	_MAXCOL_ };


	int error() { return m_errorCode; }
	int time_step_seconds();
	float time_step_hours();
	size_t num_records();
	bool has_data( int id );
	std::vector<int> get_columns();
	std::string get_canonical_name( int id );

	bool interpolate() { return m_hdrInterpMet; }
	bool has_units() { return m_hdrHasUnits; }
	std::string location() { return m_hdrLocId; }
	std::string city() { return m_hdrCity; }
	std::string state() { return m_hdrState; }
	std::string country() { return m_hdrCountry; }
	std::string source() { return m_hdrSource; }
	std::string description() { return m_hdrDescription; }
	std::string url() { return m_hdrURL; }
	double lat() { return m_hdrLatitude; }
	double lon() { return m_hdrLongitude; }
	double tz() { return m_hdrTimeZone; }
	double elev() { return m_hdrElevation; }
	int year() { return m_hdrYear; }

	 // returns NaN if value doesn't exist and can't be estimated from existing data
	float value( int id, size_t index );


	// converts a TMY2, TMY3, EPW, or SMW file to WFCSV format
	static bool convert( const std::string &input, const std::string &output );

};

#endif

