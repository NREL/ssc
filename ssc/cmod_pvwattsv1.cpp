#include "core.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"
#include "lib_pvshade.h"


#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_pvwattsv1[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",                      "local weather file path",                     "",       "",                        "Weather",      "*",                       "LOCAL_FILE",      "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                    "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "MIN=0.05,MAX=500000",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                         "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                     "Tracking mode",                               "0/1/2/3","Fixed,1Axis,2Axis,AziAxis","PVWatts",      "*",                       "MIN=0,MAX=3,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",    "E=90,S=180,W=270",        "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",    "H=0,V=90",                "PVWatts",      "naof:tilt_eq_lat",        "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                    "Tilt=latitude override",                      "0/1",    "",                        "PVWatts",      "na:tilt",                 "BOOLEAN",                                  "" },

	/* shading inputs */
	{ SSC_INPUT,        SSC_ARRAY,       "shading",                        "Shading input data array",                    "",       shading_data::format_doc,  "PVWatts",      "?",                       "",                                         "" },
	
	/* advanced parameters */
	{ SSC_INPUT,        SSC_NUMBER,      "enable_user_poa",                "Enable user-defined POA irradiance input",    "0/1",    "",                        "PVWatts",      "?=0",                     "BOOLEAN",                                  "" },
	{ SSC_INPUT,        SSC_ARRAY,       "user_poa",                       "User-defined POA irradiance",                 "W/m2",   "",                        "PVWatts",      "enable_user_poa=1",       "LENGTH=8760",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                         "Tracker rotation limit (+/- 1 axis)",         "deg",    "",                        "PVWatts",      "?=45.0",                  "MIN=1,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_noct",                         "Nominal operating cell temperature",          "C",      "",                        "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "t_ref",                          "Reference cell temperature",                  "C",      "",                        "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma",                          "Max power temperature coefficient",           "%/C",    "",                        "PVWatts",      "?=-0.5",                  "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                        "Inverter efficiency at rated power",          "frac",   "",                        "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fd",                             "Diffuse fraction",                            "0..1",   "",                        "PVWatts",      "?=1.0",                   "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "i_ref",                          "Rating condition irradiance",                 "W/m2",   "",                        "PVWatts",      "?=1000",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "poa_cutin",                      "Min reqd irradiance for operation",           "W/m2",   "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "w_stow",                         "Wind stow speed",                             "m/s",    "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	
	/* outputs */
	{ SSC_OUTPUT,       SSC_ARRAY,       "gh",                             "Global horizontal irradiance",                "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam normal irradiance",                      "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array output",                             "Wdc",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC system output",                            "Wac",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

var_info_invalid };

class cm_pvwattsv1 : public compute_module
{
public:
	
	cm_pvwattsv1()
	{
		add_var_info( _cm_vtab_pvwattsv1 );
	}

	void exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");

		weatherfile wf( file );
		if (!wf.ok()) throw exec_error("pvwattsv1", "failed to read local weather file: " + std::string(file));
					
		double dcrate = as_double("system_size");
		double derate = as_double("derate");
		int track_mode = as_integer("track_mode"); // 0, 1, 2, 3
		double azimuth = as_double("azimuth");
		double tilt = wf.lat;
		if ( !lookup("tilt_eq_lat") || !as_boolean("tilt_eq_lat") )
			tilt = fabs( as_double("tilt") );
		
		ssc_number_t *p_user_poa = 0;
		if ( as_boolean("enable_user_poa") )
		{
			size_t count = 0;
			p_user_poa = as_array("user_poa", &count);
			if (count != 8760) p_user_poa = 0;
		}

		ssc_number_t *p_gh = allocate("gh", 8760);
		ssc_number_t *p_dn = allocate("dn", 8760);
		ssc_number_t *p_df = allocate("df", 8760);
		ssc_number_t *p_tamb = allocate("tamb", 8760);
		ssc_number_t *p_wspd = allocate("wspd", 8760);

		ssc_number_t *p_dc = allocate("dc", 8760);
		ssc_number_t *p_ac = allocate("ac", 8760);
		ssc_number_t *p_tcell = allocate("tcell", 8760);
		ssc_number_t *p_poa = allocate("poa", 8760);
	
		/* PV RELATED SPECIFICATIONS */
		
		double inoct = as_double("t_noct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double reftem = as_double("t_ref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */
		double rlim = as_double("rotlim");             /* +/- rotation in degrees permitted by physical constraint of tracker */
		double fd = as_double("fd"); // diffuse fraction
		double i_ref = as_double("i_ref"); // reference irradiance for rating condition
		double poa_cutin = as_double("poa_cutin"); // minimum POA irradiance level required for any operation
		double wind_stow = as_double("w_stow"); // maximum wind speed before stowing.  stowing causes all output to be lost
		
		
		
		// check system size
		if ( dcrate < 0.1 ) dcrate = 0.1;

	// bounds of (0,09999, 0.99001) are consistent with online PVWatts http://rredc.nrel.gov/solar/codes_algs/PVWATTS/version1/US/code/pvwattsv1.cgi
	//    if ( derate < 0.09999 || derate > 0.99001 ) // Use if default ac to dc derate factor out of range
		if ( derate < 0.0 || derate > 1.0 ) // Use if default ac to dc derate factor out of range
			derate = 0.77;
        
		double pcrate = dcrate * 1000.0;      // rated output of inverter in a.c. watts; 6/29/2005
		double refpwr = dcrate * 1000.0;      // nameplate in watts; 6/29/2005

		if( track_mode < 0 || track_mode > 3 )
			track_mode = 0;
		if( tilt < 0 || tilt > 90 )
			tilt = wf.lat;
		if( azimuth < 0 || azimuth > 360 )
			azimuth = 180.0;

		
		// load the shading data from the input vector
		shading_data shad;
		if (is_assigned("shading"))
		{
			if (!shad.load( as_doublevec("shading") ))
				throw exec_error("pvwattsv1", "error parsing shading data vector (shading) - see format in meta info");
			else if (!shad.check_azal_monotonic_increase())
				throw exec_error("pvwattsv1", "azimuth and altitude values must increase monotonically in shading table");
		}

		
		double shad_skydiff_factor = 1;
		if ( shad.en_diff )
			shad_skydiff_factor = shad.diff;

		std::vector<double> shad_beam_factor(8760, 1.0);
		if ( shad.en_hourly )
			shad_beam_factor = shad.hourly;

		if ( shad.en_mxh )
		{
			int c=0;
			for (int m=0;m<12;m++)
				for (int d=0;d<util::nday[m];d++)
					for (int h=0;h<24;h++)
						shad_beam_factor[c++] *= shad.mxh.at(m,h);
		}

		bool enable_azalt_beam_shading = false;
		util::matrix_t<double> azaltvals;
		if ( shad.en_azal)
		{
			azaltvals = shad.azal;
			enable_azalt_beam_shading = true;
		}
		pvwatts_celltemp tccalc( inoct, height, 1.0 );
	
		int i=0;
		while( i < 8760 )
		{
			if (!wf.read())
				throw exec_error("pvwattsv1", "could not read data line " + util::to_string(i+1) + " of 8760 in weather file");

			irrad irr;
			irr.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, wf.step / 3600.0 );
			irr.set_location( wf.lat, wf.lon, wf.tz );
				
			double alb = 0.2;
			if (wf.snow > 0 && wf.snow < 150)
				alb = 0.6;

			irr.set_sky_model( 2, alb );
			irr.set_beam_diffuse( wf.dn, wf.df );
			irr.set_surface( track_mode, tilt, azimuth, rlim, -1, -1 );
			
			double ibeam, iskydiff, ignddiff;
			double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
			int sunup;		

			if (!irr.calc())
				sunup = 0; // if for some reason the irradiance processor fails, ignore this hour

			p_tcell[i] = (ssc_number_t)wf.tdry;

			p_gh[i] = (ssc_number_t)wf.gh;
			p_dn[i] = (ssc_number_t)wf.dn;
			p_df[i] = (ssc_number_t)wf.df;
			p_tamb[i] = (ssc_number_t)wf.tdry;
			p_wspd[i] = (ssc_number_t)wf.wspd;
	
			irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );
			if (sunup > 0)
			{
				irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
				irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);

				// apply hourly shading factors to beam (if none enabled, factors are 1.0)
				ibeam *= shad_beam_factor[i];
				
				// apply beam shading based on solar azimuth/altitude table
				if ( enable_azalt_beam_shading )
					ibeam *= util::bilinear( solalt, solazi, azaltvals );
				
				// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
				iskydiff *= shad_skydiff_factor;

				double poa = ibeam + fd*(iskydiff +ignddiff);

				if ( p_user_poa != 0 )
					poa = p_user_poa[i];

				if (poa_cutin > 0 && poa < poa_cutin)
					poa = 0;

				double wspd_corr = wf.wspd < 0 ? 0 : wf.wspd;

				if (wind_stow > 0 && wf.wspd >= wind_stow)
					poa = 0;

				double tpoa = transpoa(poa, wf.dn, aoi*3.14159265358979/180);
				double pvt = tccalc( poa, wspd_corr, wf.tdry );
				double dc = dcpowr(reftem,refpwr,pwrdgr,tmloss,tpoa,pvt,i_ref);
				double ac = dctoac(pcrate,efffp,dc);
			
				p_poa[i] = (ssc_number_t)poa;
				p_tcell[i] = (ssc_number_t)pvt;
				p_dc[i] = (ssc_number_t)dc;
				p_ac[i] = (ssc_number_t)ac;
			}
		
			i++;
		}
	}
};

DEFINE_MODULE_ENTRY( pvwattsv1, "PVWatts V.1 - integrated hourly weather reader and PV system simulator.", 1 )
