#include "core.h"
#include "lib_util.h"
#include "lib_weatherfile.h"

static var_info _cm_vtab_wfreader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "file_name",               "local weather file path",          "",       "",                      "Weather Reader",      "*",                       "LOCAL_FILE",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "header_only",             "read header only",                 "0/1",    "",                      "Weather Reader",      "?=0",                     "BOOLEAN",      "" },
	
// header data
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                     "Latitude",                         "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                     "Longitude",                        "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "tz",                      "Time zone",                        "hr",     "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "elev",                    "Elevation",                        "m",      "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "location",                "Location ID",                      "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                             "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                            "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "country",                 "Country",                          "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "description",             "Description",                      "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "source",                  "Source",                           "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "url",                     "URL",                              "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "format",                  "File format",                      "",       "tmy2,tmy3,epw,smw,wfcsv", "Weather Reader",    "*",                        "",                      "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "start",                   "Start",                            "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "step",                    "Step",                             "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "nrecords",                "Number of records",                "",       "",                      "Weather Reader",      "*",                       "",                          "" },


// timestamp data
	{ SSC_OUTPUT,        SSC_ARRAY,       "year",                    "Year",                             "yr",     "",                      "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "month",                   "Month",                            "mn",     "1-12",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "day",                     "Day",                              "dy",     "1-365",                 "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hour",                    "Hour",                             "hr",     "0-23",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "minute",                  "Minute",                           "min",    "0-59",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },

// solar & weather data records
	{ SSC_OUTPUT,        SSC_ARRAY,       "global",                  "Global Horizontal Irradiance",     "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "beam",                    "Beam Normal Irradiance",           "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "diffuse",                 "Diffuse Horizontal Irradiance",    "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },

	{ SSC_OUTPUT,        SSC_ARRAY,       "wspd",                    "Wind Speed",                       "m/s",   "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "wdir",                    "Wind Direction",                   "deg",   "0=N,E=90",               "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "tdry",                    "Temperature, Dry Bulb",            "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "twet",                    "Temperature, Wet Bulb",            "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "tdew",                    "Temperature, Dew Point",           "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "rhum",                    "Relative Humidity",                "%",     "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "pres",                    "Atmospheric Pressure",             "millibar", "",                    "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "snow",                    "Snow Depth",                       "cm",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "albedo",                  "Ground Reflectance",               "frac",  "0..1",                   "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },

// annual statistics
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_global",           "Average daily global horizontal",  "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_beam",             "Average daily beam normal",        "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_diffuse",          "Average daily diffuse",            "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_tdry",             "Average dry bulb temperature",     "'C",           "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_wspd",             "Average wind speed",               "m/s",          "",                "Weather Reader",      "header_only=0",                        "",     "" },

var_info_invalid };

class cm_wfreader : public compute_module
{
public:

	cm_wfreader()
	{
		add_var_info( _cm_vtab_wfreader );
	}
	
	void exec( ) throw( general_error )
	{
		bool header_only = as_boolean("header_only");
		const char *file = as_string("file_name");

		weatherfile wf( file, header_only );
		if( wf.msg() ) log( wf.message(), SSC_WARNING);
		if (!wf.ok()) 
		{
			assign( "error", var_data(wf.error_message()) );
			throw exec_error("wfreader", "failed to read local weather file: " + std::string(file) + "  " + wf.error_message());
		}

		int records = wf.nrecords;
		
		for (int i = 3; i < 100; i++){

		}

		assign( "lat", var_data( (ssc_number_t)wf.lat ) );
		assign( "lon", var_data( (ssc_number_t)wf.lon ) );
		assign( "tz", var_data( (ssc_number_t)wf.tz ) );
		assign( "elev", var_data( (ssc_number_t)wf.elev ) );
		assign( "location", var_data( std::string( wf.location ) ) );
		assign( "city", var_data( std::string( wf.city ) ) );
		assign( "state", var_data( std::string( wf.state ) ) );
		assign( "country", var_data( std::string( wf.country ) ) );
		assign( "description", var_data( std::string( wf.description ) ) );
		assign( "source", var_data( std::string( wf.source ) ) );
		assign( "url", var_data( std::string( wf.url ) ) );

		assign( "start", var_data( (ssc_number_t)wf.start ) );
		assign( "step", var_data( (ssc_number_t)wf.step ) );
		assign( "nrecords", var_data( (ssc_number_t)wf.nrecords ) );

		switch( wf.type() )
		{
		case weatherfile::TMY2: assign("format", var_data("tmy2") ); break;
		case weatherfile::TMY3: assign("format", var_data("tmy3") ); break;
		case weatherfile::EPW: assign("format", var_data("epw") ); break;
		case weatherfile::SMW: assign("format", var_data("smw") ); break;
		case weatherfile::WFCSV: assign("format", var_data("wfcsv") ); break;
		default: assign("format", var_data("invalid")); break;
		}

		if ( header_only )
			return;

		ssc_number_t *p_year = allocate( "year", records );
		ssc_number_t *p_month = allocate( "month", records );
		ssc_number_t *p_day = allocate( "day", records );
		ssc_number_t *p_hour = allocate( "hour", records );
		ssc_number_t *p_minute = allocate( "minute", records );
		
		ssc_number_t *p_global = allocate( "global", records );
		ssc_number_t *p_beam = allocate( "beam", records );
		ssc_number_t *p_diffuse = allocate( "diffuse", records );
		
		ssc_number_t *p_wspd = allocate( "wspd", records );
		ssc_number_t *p_wdir = allocate( "wdir", records );
		ssc_number_t *p_tdry = allocate( "tdry", records );
		ssc_number_t *p_twet = allocate( "twet", records );
		ssc_number_t *p_tdew = allocate( "tdew", records );
		ssc_number_t *p_rhum = allocate( "rhum", records );
		ssc_number_t *p_pres = allocate( "pres", records );
		ssc_number_t *p_snow = allocate( "snow", records );
		ssc_number_t *p_albedo = allocate( "albedo", records );

		double gh_sum = 0.0, dn_sum = 0.0, df_sum = 0.0;
		double temp_sum = 0.0, wind_sum = 0.0;

		double ts_hour = wf.step / 3600.0;

		for (int i=0;i<records;i++)
		{
			if (!wf.read())
				throw exec_error("wfreader", "could not read data line " + util::to_string(i+1) + " of 8760");

			p_year[i] = (ssc_number_t)wf.year;
			p_month[i] = (ssc_number_t)wf.month;
			p_day[i] = (ssc_number_t)wf.day;
			p_hour[i] = (ssc_number_t)wf.hour;
			p_minute[i] = (ssc_number_t)wf.minute;

			p_global[i] = (ssc_number_t)wf.gh;
			p_beam[i] = (ssc_number_t)wf.dn;
			p_diffuse[i] = (ssc_number_t)wf.df;

			p_wspd[i] = (ssc_number_t)wf.wspd;
			p_wdir[i] = (ssc_number_t)wf.wdir;
			p_tdry[i] = (ssc_number_t)wf.tdry;
			p_twet[i] = (ssc_number_t)wf.twet;
			p_tdew[i] = (ssc_number_t)wf.tdew;
			p_rhum[i] = (ssc_number_t)wf.rhum;
			p_pres[i] = (ssc_number_t)wf.pres;
			p_snow[i] = (ssc_number_t)wf.snow;
			p_albedo[i] = (ssc_number_t)wf.albedo;	

			gh_sum += wf.gh * ts_hour;
			dn_sum += wf.dn * ts_hour;
			df_sum += wf.df * ts_hour;
			temp_sum += wf.tdry;
			wind_sum += wf.wspd; 
		}
		
		assign( "annual_global", var_data( 0.001 * gh_sum / 365 ));
		assign( "annual_beam", var_data( 0.001 * dn_sum / 365 ));
		assign( "annual_diffuse", var_data( 0.001 * df_sum / 365 ));
		assign( "annual_tdry", var_data( temp_sum / records ));
		assign( "annual_wspd", var_data( wind_sum / records ));
	}
};

DEFINE_MODULE_ENTRY( wfreader, "Standard Weather File Format Reader (TMY2, TMY3, EPW, SMW, WFCSV)", 1 )
