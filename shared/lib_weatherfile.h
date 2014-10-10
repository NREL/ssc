#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>
#include <vector>  // needed to compile in typelib_vc2012



/***************************************************************************\

   Function humidity()
	This function calculates the relative humidity(%) based on the drybulb
	temperature(C) and the dewpoint temperature.  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If humidity cannot be calculated an error value of 999 is returned.
														  1/4/00
	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	dpt    = dew point temperature in degrees C

	Variable Returned
	rh    = relative humidity in %, or error value of 999  

\***************************************************************************/
int calc_humidity(float db,float dpt);

/* This function calculates the dewpoint temperature(C) based on the drybulb
	temperature(C) and the relative humidity(%).  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If dewpoint cannot be calculated an error value of 99.9 is returned.

	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	rh     = relative humidity in %

	Variable Returned
	dpt    = dew point temperature in degrees C, or error value of 99.9   */
float calc_dewpt(float db,float rh);

// Calculate wet bulb temperature from T (dry bulb, 'C), RH (%), Pressure (mbar)
// see http://www.ejournal.unam.mx/atm/Vol07-3/ATM07304.pdf for eqns.
double calc_twet( double T, double RH, double P );


class weatherfile
{
private:
	bool m_ok;
	int m_type;
	std::string m_file;
	int m_startYear;
	double m_time;
	std::string m_errorStr;

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
	std::string error_message() { return m_errorStr; }

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

