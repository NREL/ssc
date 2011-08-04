#include "core.h"

#include "lib_wfhrly.h"
#include "lib_pvwatts.h"

static var_info _cm_vtab_easywatts[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "year",                       "Year (defaults to 1990)",        "",       "",                      "Weather",      "?=1990",                  "INTEGER,MIN=1950",                         "" },
	{ SSC_INPUT,        SSC_STRING,      "file_name",                  "local weather file path",        "",       "",                      "Weather",      "*",                       "LOCAL_FILE",      "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                            "MIN=0.05,MAX=500000",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "System derate value",            "frac",   "",                      "PVWatts",      "*",                            "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                 "Tracking mode",                  "0/1/2",  "Fixed,1Axis,2Axis",     "PVWatts",      "*",                            "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                    "Azimuth angle",                  "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                            "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                       "Tilt angle",                     "deg",    "H=0,V=90",              "PVWatts",      "naof:tilt_eq_lat",             "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "na:tilt",                      "BOOLEAN",                                  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                        "Plane of array radiation",       "W/m2",   "",                      "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                      "Module temperature",             "'C",     "",                      "PVWatts",      "*",                       "LENGTH=8760",                          "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                         "DC array output",                "kWhdc",  "",                      "PVWatts",      "*",                            "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                         "AC system output",               "kWhac",  "",                      "PVWatts",      "*",                            "LENGTH=8760",                          "" },

var_info_invalid };

class cm_easywatts : public compute_module
{
public:

	class weather_reader
	{
	public:
		weather_reader() : wf(0) {  }
		~weather_reader() { if (wf) wf_close(wf); }
		wf_obj_t wf;
	};

	cm_easywatts()
	{
		add_var_info( _cm_vtab_easywatts );
	}

	void exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");

		wf_header hdr;
		wf_data dat;
		weather_reader reader;
		reader.wf = wf_open( file, &hdr );

		if (!reader.wf) throw exec_error("easywatts", "failed to read local weather file: " + std::string(file));
			
		int year = as_integer("year");
		
		double watt_spec = 1000.0 * as_double("system_size");
		double derate = as_double("derate");
		int track_mode = as_integer("track_mode"); // 0, 1, 2
		double azimuth = as_double("azimuth");
		double tilt = hdr.lat;
		if ( !lookup("tilt_eq_lat") || !as_boolean("tilt_eq_lat") )
			tilt = as_double("tilt");

		ssc_number_t *p_dc = allocate("dc", 8760);
		ssc_number_t *p_ac = allocate("ac", 8760);
		ssc_number_t *p_tcell = allocate("tcell", 8760);
		ssc_number_t *p_poa = allocate("poa", 8760);
	
		/* PV RELATED SPECIFICATIONS */
		double inoct = PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double reftem = PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */
		double rot_limit = PVWATTS_ROTLIM;             /* +/- rotation in degrees permitted by physical constraint of tracker */
		double albedo = PVWATTS_ALBEDO;                 /* surface albedo, decimal fraction */

		/* storage for calculations */
		double angle[3];
		double sun[8];
		
		for (int i=0;i<8760;i++)
		{
			if (!wf_read_data( reader.wf, &dat ))
				throw exec_error("easywatts", "could not read data line " + util::to_string(i+1) + " of 8760");

			int month = util::month_of((float)i) ;              // month goes 1-12
			int day = util::day_of_month(month,(float)i) ;   // day goes 1-nday_in_month
			int hour = i%24;		         // hour goes 0-23
			
			// calculate solar position
			solarpos( year, month, day, hour, 30, hdr.lat, hdr.lon, hdr.tz, sun );

			double poa, pvt, dc, ac;
			if (sun[2] > 0.0087)
			{
				/* sun elevation > 0.5 degrees */
				incident2( track_mode, tilt, azimuth, rot_limit, sun[1], sun[0], angle );
				poa = perez( dat.dn, dat.df, albedo, angle[0], angle[1], sun[1] );

				double tpoa = 0;
				if (dat.dn > 0)	
					tpoa = transpoa( poa, dat.dn, angle[0] );  /* have valid poa and dn, calculate transmitted through glass cover */
				else
					tpoa = poa; /* default to dn 0 or bad value - assume no glass cover on module */
				
				pvt = celltemp(inoct, height, poa, dat.wspd, dat.tdry );
				dc = dcpowr( reftem, watt_spec, pwrdgr, tmloss, tpoa, pvt );
				ac = dctoac( watt_spec, efffp, dc );
			}
			else
			{
				/* night time */
				poa = 0.0;
				pvt = 999.9;
				dc = 0.0;
				ac = 0.0;
			}

			p_poa[i] = (ssc_number_t)poa;
			p_tcell[i] = (ssc_number_t)pvt;
			p_dc[i] = (ssc_number_t)dc;
			p_ac[i] = (ssc_number_t)ac;
		}
	}
};

DEFINE_MODULE_ENTRY( easywatts, "EasyWatts - PVWatts based integrated hourly weather reader and PV system simulator.", 1 )
