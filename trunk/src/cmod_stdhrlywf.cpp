#include "core.h"

#include "lib_wfhrly.h"

static var_info _cm_vtab_stdhrlywf[] = {
/*    VARTYPE     DATATYPE      NAME           LABEL                            UNITS    META                      GROUP         REQUIRED_IF                CONSTRAINTS       UI_HINTS*/
	{ SSC_INPUT,  SSC_STRING,   "file_name",   "local weather file path",       "",      "",                       "",           "*",                       "LOCAL_FILE",      "" },
	{ SSC_INPUT,  SSC_NUMBER,   "header_only", "only parse the header info?",   "",      "",                       "",           "?",                       "BOOLEAN",         "" },

	
	{ SSC_OUTPUT, SSC_STRING,   "format",      "file format type",              "",      "values: tmy2,tmy3,epw",  "Header",     "*",                       "",                "hide" },
	{ SSC_OUTPUT, SSC_STRING,   "loc_id",      "location id",                   "",      "",                       "Header",     "*",                       "",                "hide" },
	{ SSC_OUTPUT, SSC_STRING,   "loc_text",    "location description",          "",      "",                       "Header",     "*",                       "",                "hide" },
	{ SSC_OUTPUT, SSC_NUMBER,   "lat",         "latitude",                      "deg",   "",                       "Header",     "*",                       "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "lon",         "longitude",                     "deg",   "",                       "Header",     "*",                       "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "tz",          "timezone relative to gmt",      "hrs",   "",                       "Header",     "*",                       "",                "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "elev",        "elevation",                     "m",     "",                       "Header",     "*",                       "",                "" },

	{ SSC_OUTPUT, SSC_ARRAY,    "gh",          "global horizontal insolation",  "W/m2",  "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "dn",          "direct normal insolation",      "W/m2",  "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "df",          "diffuse insolation",            "W/m2",  "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "tdry",        "dry-bulb temp",                 "'C",    "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "twet",        "wet-bulb temp",                 "'C",    "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "wspd",        "wind speed",                    "m/s",   "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "wdir",        "wind direction",                "deg",   "N=0,E=90,S=180,W=270",   "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "rhum",        "relative humidity",             "%",     "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },
	{ SSC_OUTPUT, SSC_ARRAY,    "pres",        "pressure",                      "mbar",  "",                       "DataSet",    "abf:header_only",         "LENGTH=8760",     "" },

var_info_invalid };

class cm_stdhrlywf : public compute_module
{
private:
public:
	cm_stdhrlywf()
	{
		add_var_info( _cm_vtab_stdhrlywf );
	}

	bool exec( ) throw( general_error )
	{
		bool header_only = false;
		if (lookup("header_only"))
			header_only = as_boolean("header_only");

		const char *file = as_string("file_name");

		wf_header hdr;
		wf_data dat;
		wf_obj_t wf = wf_open( file, &hdr );

		if (!wf) throw exec_error("stdhrlywf", "failed to read local weather file: " + std::string(file));
	

		switch(hdr.type)
		{
		case WF_TMY2: assign("format", var_data("tmy2")); break;
		case WF_TMY3: assign("format", var_data("tmy3")); break;
		case WF_EPW: assign("format", var_data("epw")); break;
		default:
			wf_close(wf);
			throw exec_error("stdhrlywf", "could not determine standard weather file format: " + std::string(file));
		}

		assign("loc_id", var_data(hdr.loc_id));
		assign("loc_text", var_data( std::string(hdr.city) + " " + std::string(hdr.state)));
		assign("lat", var_data( (ssc_number_t)hdr.lat));
		assign("lon", var_data( (ssc_number_t)hdr.lon));
		assign("tz", var_data( (ssc_number_t)hdr.tz));
		assign("elev", var_data( (ssc_number_t)hdr.elev));

		if (!header_only)
		{
			ssc_number_t *p_gh = allocate("gh", 8760);
			ssc_number_t *p_dn = allocate("dn", 8760);
			ssc_number_t *p_df = allocate("df", 8760);
			ssc_number_t *p_tdry = allocate("tdry", 8760);
			ssc_number_t *p_twet = allocate("twet", 8760);
			ssc_number_t *p_wspd = allocate("wspd", 8760);
			ssc_number_t *p_wdir = allocate("wdir", 8760);
			ssc_number_t *p_rhum = allocate("rhum", 8760);
			ssc_number_t *p_pres = allocate("pres", 8760);
		
			for (int i=0;i<8760;i++)
			{
				if (!wf_read_data( wf, &dat ))
				{
					wf_close(wf);
					throw exec_error("stdhrlywf", "could not read data line " + util::to_string(i+1) + " of 8760");
				}

				if (i%876 == 0)
					update("reading", 100.0f * float(i+1) / 8760.0f, (float)i);

				p_gh[i] = (ssc_number_t) dat.gh;
				p_dn[i] = (ssc_number_t) dat.dn;
				p_df[i] = (ssc_number_t) dat.df;
				p_tdry[i] = (ssc_number_t) dat.tdry;
				p_twet[i] = (ssc_number_t) dat.twet;
				p_wspd[i] = (ssc_number_t) dat.wspd;
				p_wdir[i] = (ssc_number_t) dat.wdir;
				p_rhum[i] = (ssc_number_t) dat.rhum;
				p_pres[i] = (ssc_number_t) dat.pres;
			}
		}

		wf_close(wf);

		return true;
	}
};

DEFINE_MODULE_ENTRY( stdhrlywf, "Standard format hourly weather file reader. Supports TMY2, TMY3, EPW", 1 )

