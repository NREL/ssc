#include "core.h"
#include "lib_util.h"
#include "lib_weatherfile.h"

static var_info _cm_vtab_wfreader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "file_name",               "local weather file path",          "",       "",                      "Weather Reader",      "*",                       "LOCAL_FILE",      "" },
	
// header data
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                     "Latitude",                         "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                     "Longitude",                        "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "tz",                      "Time zone",                        "hr",     "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "elev",                    "Elevation",                        "m",      "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "location",                "Location ID",                      "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                             "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                            "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "start",                   "Start",                            "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "step",                    "Step",                             "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "nrecords",                "Number of records",                "",       "",                      "Weather Reader",      "*",                       "",                          "" },


// timestamp data
	{ SSC_OUTPUT,        SSC_ARRAY,       "year",                    "Year",                             "yr",     "",                      "Weather Reader",      "*",                       "",               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "month",                   "Month",                            "mn",     "1-12",                  "Weather Reader",      "*",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "day",                     "Day",                              "dy",     "1-365",                 "Weather Reader",      "*",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hour",                    "Hour",                             "hr",     "0-23",                  "Weather Reader",      "*",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "minute",                  "Minute",                           "min",    "0-59",                  "Weather Reader",      "*",                       "LENGTH_EQUAL=year",                          "" },

// solar & weather data records
	{ SSC_OUTPUT,        SSC_ARRAY,       "global",                  "Global Horizontal Irradiance",     "W/m2",   "",                      "Weather Reader",      "*",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "beam",                    "Beam Normal Irradiance",           "W/m2",   "",                      "Weather Reader",      "*",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "diffuse",                 "Diffuse Horizontal Irradiance",    "W/m2",   "",                      "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },

	{ SSC_OUTPUT,        SSC_ARRAY,       "wspd",                    "Wind Speed",                       "m/s",   "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "wdir",                    "Wind Direction",                   "deg",   "0=N,E=90",               "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "tdry",                    "Temperature, Dry Bulb",            "'C",    "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "twet",                    "Temperature, Wet Bulb",            "'C",    "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "rhum",                    "Relative Humidity",                "%",     "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "pres",                    "Atmospheric Pressure",             "millibar", "",                    "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "snow",                    "Snow Depth",                       "cm",    "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "albedo",                  "Ground Reflectance",               "frac",  "0..1",                   "Weather Reader",      "*",                        "LENGTH_EQUAL=year",     "" },

var_info_invalid };

class cm_wfreader : public compute_module
{
public:

	class weather_reader
	{
	public:
		weather_reader() : wf(0) {  }
		~weather_reader() { if (wf) wf_close(wf); }
		wf_obj_t wf;
	};

	cm_wfreader()
	{
		add_var_info( _cm_vtab_wfreader );
	}
	
	void exec( ) throw( general_error )
	{	
		const char *file = as_string("file_name");

		wf_header_t hdr;
		wf_record_t dat;
		weather_reader reader;
		reader.wf = wf_open( file, &hdr );

		int records = hdr.nrecords;

		if (!reader.wf) throw exec_error("wfreader", "failed to read local weather file: " + std::string(file));
		
		assign( "lat", var_data( (ssc_number_t)hdr.lat ) );
		assign( "lon", var_data( (ssc_number_t)hdr.lon ) );
		assign( "tz", var_data( (ssc_number_t)hdr.tz ) );
		assign( "elev", var_data( (ssc_number_t)hdr.elev ) );
		assign( "location", var_data( std::string( hdr.loc_id ) ) );
		assign( "city", var_data( std::string( hdr.city ) ) );
		assign( "state", var_data( std::string( hdr.state ) ) );

		assign( "start", var_data( (ssc_number_t)hdr.start ) );
		assign( "step", var_data( (ssc_number_t)hdr.step ) );
		assign( "nrecords", var_data( (ssc_number_t)hdr.nrecords ) );

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
		ssc_number_t *p_rhum = allocate( "rhum", records );
		ssc_number_t *p_pres = allocate( "pres", records );
		ssc_number_t *p_snow = allocate( "snow", records );
		ssc_number_t *p_albedo = allocate( "albedo", records );

		for (int i=0;i<records;i++)
		{
			if (!wf_read_data( reader.wf, &dat ))
				throw exec_error("wfreader", "could not read data line " + util::to_string(i+1) + " of 8760");

			p_year[i] = (ssc_number_t)dat.year;
			p_month[i] = (ssc_number_t)dat.month;
			p_day[i] = (ssc_number_t)dat.day;
			p_hour[i] = (ssc_number_t)dat.hour;
			p_minute[i] = (ssc_number_t)dat.minute;

			p_global[i] = (ssc_number_t)dat.gh;
			p_beam[i] = (ssc_number_t)dat.dn;
			p_diffuse[i] = (ssc_number_t)dat.df;

			p_wspd[i] = (ssc_number_t)dat.wspd;
			p_wdir[i] = (ssc_number_t)dat.wdir;
			p_tdry[i] = (ssc_number_t)dat.tdry;
			p_twet[i] = (ssc_number_t)dat.twet;
			p_rhum[i] = (ssc_number_t)dat.rhum;
			p_pres[i] = (ssc_number_t)dat.pres;
			p_snow[i] = (ssc_number_t)dat.snow;
			p_albedo[i] = (ssc_number_t)dat.albedo;			
		}
	}
};

DEFINE_MODULE_ENTRY( wfreader, "Standard Weather File Format Reader (TMY2, TMY3, EPW, SMW)", 1 )
