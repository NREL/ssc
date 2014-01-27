#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>
#include <vector>  // needed to compile in typelib_vc2012


class weatherfile
{
private:
	bool m_ok;
	int m_type;
	std::string m_file;
	int m_startYear;
	double m_time;
	int m_errorLine;
	
	enum { YEAR, MONTH, DAY, HOUR, MINUTE,
		GHI, DNI, DHI, 
		TDRY, TWET, TDEW, 
		WSPD, WDIR, 
		RH, PRES, SNOW, ALB, AOD,
	_MAXCOL_ };

	struct column
	{
		int index; // used for wfcsv to get column index in CSV file from which to read
		std::vector<float> data;
	};
	
	column m_columns[_MAXCOL_];
	size_t m_index;


	void reset_record();

public:
	weatherfile();
	weatherfile( const std::string &file, bool header_only = false, bool interp = false );

	void reset();

	enum { INVALID, TMY2, TMY3, EPW, SMW, WFCSV };

	bool ok();
	int type();
	std::string filename();
	bool open( const std::string &file, bool header_only = false, bool interp = false );
	void rewind();

	static std::string normalize_city( const std::string &in );
	static bool convert_to_wfcsv( const std::string &input, const std::string &output );

	/******** header data *******/
	std::string location;
	std::string city;
	std::string state;
	std::string country;
	std::string source;
	std::string description;
	std::string url;
	bool interpmet;
	bool hasunits;
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
	double aod; /* aerosol optical depth */

};


#endif

