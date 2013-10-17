#include "core.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_pvwatts.h"
#include "lib_pvshade.h"

#ifndef DTOR
#define DTOR 0.0174532925
#endif
#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
#define sind(x) sin( (M_PI/180.0)*(x) )
#define cosd(x) cos( (M_PI/180.0)*(x) )

static var_info _cm_vtab_pvwattsv1[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",                      "local weather file path",                     "",       "",                        "Weather",      "*",                       "LOCAL_FILE",      "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                         "Albedo (ground reflectance)",                 "frac",   "",                        "PVWatts",      "?",                       "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                    "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "MIN=0.05,MAX=500000",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                         "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                     "Tracking mode",                               "0/1/2/3","Fixed,1Axis,2Axis,AziAxis","PVWatts",      "*",                       "MIN=0,MAX=3,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",    "E=90,S=180,W=270",        "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",    "H=0,V=90",                "PVWatts",      "naof:tilt_eq_lat",        "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt_eq_lat",                    "Tilt=latitude override",                      "0/1",    "",                        "PVWatts",      "na:tilt",                 "BOOLEAN",                                  "" },

	/* shading inputs */
	{ SSC_INPUT,        SSC_ARRAY,       "shading_hourly",                 "Hourly beam shading factors",                 "",       "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading_mxh",                    "Month x Hour beam shading factors",           "",       "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading_azal",                   "Azimuth x altitude beam shading factors",     "",       "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading_diff",                   "Diffuse shading factor",                      "",       "",                        "PVWatts",      "?",                        "",                              "" },
		
	/* advanced parameters */
	{ SSC_INPUT,        SSC_NUMBER,      "enable_user_poa",                "Enable user-defined POA irradiance input",    "0/1",    "",                        "PVWatts",      "?=0",                     "BOOLEAN",                                  "" },
	{ SSC_INPUT,        SSC_ARRAY,       "user_poa",                       "User-defined POA irradiance",                 "W/m2",   "",                        "PVWatts",      "enable_user_poa=1",       "LENGTH=8760",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotlim",                         "Tracker rotation limit (+/- 1 axis)",         "deg",    "",                        "PVWatts",      "?=45.0",                  "MIN=1,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inoct",                          "Nominal operating cell temperature",          "C",      "",                        "PVWatts",      "?=45.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tref",                           "Reference cell temperature",                  "C",      "",                        "PVWatts",      "?=25.0",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gamma",                          "Max power temperature coefficient",           "%/C",    "",                        "PVWatts",      "?=-0.5",                  "",                                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                        "Inverter efficiency at rated power",          "frac",   "",                        "PVWatts",      "?=0.92",                  "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fd",                             "Diffuse fraction",                            "0..1",   "",                        "PVWatts",      "?=1.0",                   "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "i_ref",                          "Rating condition irradiance",                 "W/m2",   "",                        "PVWatts",      "?=1000",                  "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "poa_cutin",                      "Min reqd irradiance for operation",           "W/m2",   "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "w_stow",                         "Wind stow speed",                             "m/s",    "",                        "PVWatts",      "?=0",                     "MIN=0",                                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "concen",                         "Concentration ratio",                         "",       "",                        "PVWatts",      "?=1",                     "MIN=1",                                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fhconv",                         "Convective heat transfer factor",             "",       "",                        "PVWatts",      "?=1",                     "MIN=0.1",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shade_mode_1x",                  "Tracker self-shading mode",                   "0/1/2",  "0=shading,1=backtrack,2=none","PVWatts",  "?=2",                     "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gcr",                            "Ground coverage ratio",                       "0..1",   "",                            "PVWatts",  "?=0.3",                   "MIN=0,MAX=3",               "" },
	

	
	/* outputs */

	{ SSC_OUTPUT,       SSC_ARRAY,       "gh",                             "Global horizontal irradiance",                "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tdew",                           "Dew point temperature",                       "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array output",                             "Wdc",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC system output",                            "Wac",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "shad_beam_factor",               "Shading factor for beam radiation",           "",       "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "sunup",                          "Sun up over horizon",                         "0/1",    "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_monthly",                    "Plane of array irradiance",                   "kWh/m2",   "",                        "PVWatts",      "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solrad_monthly",                 "Daily average solar irradiance",              "kWh/m2/day","",                        "PVWatts",      "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_monthly",                     "DC array output",                             "kWhdc",    "",                        "PVWatts",      "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac_monthly",                     "AC system output",                            "kWhac",    "",                        "PVWatts",      "*",                       "LENGTH=12",                          "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "solrad_annual",                  "Daily average solar irradiance",              "kWh/m2/day",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ac_annual",                      "Annual AC system output",                     "kWhac",    "",                        "PVWatts",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_STRING,      "location",                      "Location ID",                                  "",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "city",                          "City",                                         "",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "state",                         "State",                                        "",    "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lat",                           "Latitude",                                     "deg", "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lon",                           "Longitude",                                    "deg", "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tz",                            "Time zone",                                    "hr",  "",                        "PVWatts",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "elev",                          "Site elevation",                               "m",   "",                        "PVWatts",      "*",                       "",                          "" },


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
		ssc_number_t *p_tdew = allocate("tdew", 8760);
		ssc_number_t *p_wspd = allocate("wspd", 8760);

		ssc_number_t *p_dc = allocate("dc", 8760);
		ssc_number_t *p_ac = allocate("ac", 8760);
		ssc_number_t *p_tcell = allocate("tcell", 8760);
		ssc_number_t *p_poa = allocate("poa", 8760);

		ssc_number_t *p_shad_beam_factor = allocate("shad_beam_factor", 8760);
		ssc_number_t *p_sunup = allocate("sunup", 8760);
	
		/* PV RELATED SPECIFICATIONS */
		
		double inoct = as_double("inoct") + 273.15; // PVWATTS_INOCT;        /* Installed normal operating cell temperature (deg K) */
		double reftem = as_double("tref"); // PVWATTS_REFTEM;                /* Reference module temperature (deg C) */
		double pwrdgr = as_double("gamma") / 100.0; // PVWATTS_PWRDGR;              /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
		double efffp = as_double("inv_eff"); // PVWATTS_EFFFP;                 /* Efficiency of inverter at rated output (decimal fraction) */

		double height = PVWATTS_HEIGHT;                 /* Average array height (meters) */
		double tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */
		double rlim = as_double("rotlim");             /* +/- rotation in degrees permitted by physical constraint of tracker */
		double fd = as_double("fd"); // diffuse fraction
		double i_ref = as_double("i_ref"); // reference irradiance for rating condition
		double poa_cutin = as_double("poa_cutin"); // minimum POA irradiance level required for any operation
		double wind_stow = as_double("w_stow"); // maximum wind speed before stowing.  stowing causes all output to be lost
		double concen = 1.0;
		if ( is_assigned("concen") ) concen = as_double("concen"); // concentration ratio.  used to increase incident irradiance on cells for thermal calculaton		
		double fhconv = 1.0;
		if ( is_assigned("fhconv") ) fhconv = as_double("fhconv"); // convective heat transfer coefficient factor.  used to approximate effect of a heatsink for lcpv
		int shade_mode_1x = 2; // no self shading on 1 axis tracker
		if ( is_assigned("shade_mode_1x") ) shade_mode_1x = as_integer("shade_mode_1x");
		double gcr = 0.3;
		if ( is_assigned("gcr") ) gcr = as_double("gcr");

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
						
		
//		initialize to no shading
		for ( size_t j=0;j<8760;j++)
			p_shad_beam_factor[j] = 1.0;

		if ( is_assigned("shading_hourly" ) )
		{
			size_t len = 0;
			ssc_number_t *vals = as_array( "shading_hourly", &len );
			if ( len == 8760 )
			{
				for ( size_t j=0;j<8760;j++)
					p_shad_beam_factor[j] = vals[j];
			}
			else
				throw exec_error("pvwattsv1", "hourly shading beam factors must have 8760 values");
		}


		if ( is_assigned( "shading_mxh" ) )
		{
			size_t nrows, ncols;
			ssc_number_t *mat = as_matrix( "shading_mxh", &nrows, &ncols );
			if ( nrows != 12 || ncols != 24 )
				throw exec_error("pvwattsv1", "month x hour shading factors must have 12 rows and 24 columns");

			int c=0;
			for (int m=0;m<12;m++)
				for (int d=0;d<util::nday[m];d++)
					for (int h=0;h<24;h++)
						p_shad_beam_factor[c++] *= mat[ m*ncols + h ];
		}

		bool enable_azalt_beam_shading = false;
		util::matrix_t<double> azaltvals;
		if ( is_assigned( "shading_azal" ) )
		{
			size_t nrows, ncols;
			ssc_number_t *mat = as_matrix( "shading_azal", &nrows, &ncols );
			if ( nrows < 3 || ncols < 3 )
				throw exec_error("pvwattsv1", "azimuth x altitude shading factors must have at least 3 rows and 3 columns");

			azaltvals.resize_fill( nrows, ncols, 1.0 );
			for ( size_t r=0;r<nrows;r++ )
				for ( size_t c=0;c<ncols;c++ )
					azaltvals.at(r,c) = mat[r*ncols+c];

			enable_azalt_beam_shading = true;
		}

		double shad_skydiff_factor = 1.0;
		if ( is_assigned( "shading_diff" ) )
			shad_skydiff_factor = as_double( "shading_diff" );

		pvwatts_celltemp tccalc( inoct, height, 1.0 );
		
		double fixed_albedo = 0.2;
		bool has_albedo = is_assigned( "albedo" );
		if ( has_albedo )
			fixed_albedo = as_double( "albedo" );

	
		int i=0;
		while( i < 8760 )
		{
			if (!wf.read())
				throw exec_error("pvwattsv1", "could not read data line " + util::to_string(i+1) + " of 8760 in weather file");

			irrad irr;
			irr.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, wf.step / 3600.0 );
			irr.set_location( wf.lat, wf.lon, wf.tz );
				
			double alb = 0.2;
			
			if ( has_albedo && fixed_albedo >= 0 && fixed_albedo <= 1.0 )
			{
				alb = fixed_albedo;
			}
			else if ( wf.type() == weatherfile::TMY2 )
			{
				if (wf.snow > 0 && wf.snow < 150)
					alb = 0.6;
			}
			else if ( wf.type() == weatherfile::TMY3 )
			{
				if ( wf.albedo >= 0 && wf.albedo < 1 )
					alb = wf.albedo;
			}

			irr.set_sky_model( 2, alb );
			irr.set_beam_diffuse( wf.dn, wf.df );
			irr.set_surface( track_mode, tilt, azimuth, rlim, 
				shade_mode_1x == 1, // backtracking mode
				gcr );
			
			double ibeam, iskydiff, ignddiff;
			double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
			int sunup;		

			int code = irr.calc();
			if ( 0 != code )
				sunup = 0; // if for some reason the irradiance processor fails, ignore this hour

			p_sunup[i] = (ssc_number_t)sunup;

			p_tcell[i] = (ssc_number_t)wf.tdry;

			p_gh[i] = (ssc_number_t)wf.gh;
			p_dn[i] = (ssc_number_t)wf.dn;
			p_df[i] = (ssc_number_t)wf.df;
			p_tamb[i] = (ssc_number_t)wf.tdry;
			p_tdew[i] = (ssc_number_t)wf.tdew;
			p_wspd[i] = (ssc_number_t)wf.wspd;
	
			irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );
			if (sunup > 0)
			{
				irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
				irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);
				
				if ( sunup > 0 && track_mode == 1
					&& shade_mode_1x == 0 ) // selfshaded mode
				{	
					double shad1xf = shade_fraction_1x( solazi, solzen, tilt, azimuth, gcr, rot );					
					p_shad_beam_factor[i] *= (ssc_number_t)(1-shad1xf);

					if ( fd > 0 && shade_mode_1x == 0 && iskydiff > 0 )
					{
						double reduced_skydiff = iskydiff;
						double Fskydiff = 1.0;
						double reduced_gnddiff = ignddiff;
						double Fgnddiff = 1.0;
						
						// worst-case mask angle using calculated surface tilt
						double phi0 = 180/3.1415926*atan2( sind( stilt ), 1/gcr - cosd( stilt ) );

						// calculate sky and gnd diffuse derate factors
						// based on view factor reductions from self-shading
						diffuse_reduce( solzen, stilt,
							wf.dn, iskydiff+ignddiff,
							gcr, phi0, alb, 1000,

							// outputs (pass by reference)
							reduced_skydiff, Fskydiff,
							reduced_gnddiff, Fgnddiff );

						if ( Fskydiff >= 0 && Fskydiff <= 1 ) iskydiff *= Fskydiff;
						else log( util::format("sky diffuse reduction factor invalid at hour %d: fskydiff=%lg, stilt=%lg", i, Fskydiff, stilt), SSC_NOTICE, (float)i );

						if ( Fgnddiff >= 0 && Fgnddiff <= 1 ) ignddiff *= Fgnddiff;
						else log( util::format("gnd diffuse reduction factor invalid at hour %d: fgnddiff=%lg, stilt=%lg", i, Fgnddiff, stilt), SSC_NOTICE, (float)i );
					}

				}

				// apply hourly shading factors to beam (if none enabled, factors are 1.0)
				ibeam *= p_shad_beam_factor[i];
				
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
				double pvt = tccalc( poa*concen, wspd_corr, wf.tdry, fhconv );
				double dc = dcpowr(reftem,refpwr,pwrdgr,tmloss,tpoa,pvt,i_ref);
				double ac = dctoac(pcrate,efffp,dc);
			
				p_poa[i] = (ssc_number_t)poa;
				p_tcell[i] = (ssc_number_t)pvt;
				p_dc[i] = (ssc_number_t)dc;
				p_ac[i] = (ssc_number_t)ac;
			}
		
			i++;
		}

		ssc_number_t *poam = accumulate_monthly( "poa", "poa_monthly", 0.001 );
		accumulate_monthly( "dc", "dc_monthly", 0.001 );
		accumulate_monthly( "ac", "ac_monthly", 0.001 );

		ssc_number_t *solrad = allocate( "solrad_monthly", 12 );
		ssc_number_t solrad_ann = 0;
		for ( int m=0;m<12;m++ )
		{
			solrad[m] = poam[m]/util::nday[m];
			solrad_ann += solrad[m];
		}
		assign( "solrad_annual", var_data( solrad_ann/12 ) );


		accumulate_annual( "ac", "ac_annual", 0.001 );

		assign( "location", var_data( wf.loc_id ) );
		assign( "city", var_data( wf.city ) );
		assign( "state", var_data( wf.state ) );
		assign( "lat", var_data( (ssc_number_t)wf.lat ) );
		assign( "lon", var_data( (ssc_number_t)wf.lon ) );
		assign( "tz", var_data( (ssc_number_t)wf.tz ) );
		assign( "elev", var_data( (ssc_number_t)wf.elev ) );
	}
};

DEFINE_MODULE_ENTRY( pvwattsv1, "PVWatts V.1 - integrated hourly weather reader and PV system simulator.", 2 )
