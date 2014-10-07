#include "core.h"

#include "common.h"

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
#define tand(x) tan( (M_PI/180.0)*(x) )
#define asind(x) (180/M_PI*asin(x))

static var_info _cm_vtab_pvwattsv5[] = {
/*   VARTYPE           DATATYPE          NAME                         LABEL                                               UNITS        META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",            "Weather file path",                           "",          "",                       "Weather", "*", "LOCAL_FILE", "" },

	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",                "System size (DC nameplate)",                  "kW",        "",                           "PVWatts",      "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "module_type",                    "Module type",                                 "0/1/2",     "Standard,Premium,Thin film", "PVWatts",      "?=0",                       "MIN=0,MAX=2,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "dc_ac_ratio",                    "DC to AC ratio",                              "ratio",     "",                           "PVWatts",      "?=1.1",                   "POSITIVE",                                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_eff",                        "Inverter efficiency at rated power",          "%",         "",                           "PVWatts",      "?=96",                        "MIN=90,MAX=99.5",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "losses",                         "System losses",                               "%",         "Total system losses",                               "PVWatts",      "*",                       "MIN=-5,MAX=99",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "array_type",                     "Array type",                                  "0/1/2/3/4", "Fixed OR,Fixed Roof,1Axis,Backtracked,2Axis",  "PVWatts",      "*",                       "MIN=0,MAX=4,INTEGER",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                           "Tilt angle",                                  "deg",       "H=0,V=90",                                     "PVWatts",      "*",                       "MIN=0,MAX=90",                             "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                        "Azimuth angle",                               "deg",       "E=90,S=180,W=270",                             "PVWatts",      "*",                       "MIN=0,MAX=360",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "gcr",                            "Ground coverage ratio",                       "0..1",      "",                                             "PVWatts",      "?=0.4",                   "MIN=0,MAX=3",               "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "shading:hourly",                 "Hourly beam shading loss",                    "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:mxh",                    "Month x Hour beam shading loss",              "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,      "shading:azal",                   "Azimuth x altitude beam shading loss",        "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "shading:diff",                   "Diffuse shading loss",                        "%",         "",                        "PVWatts",      "?",                        "",                              "" },
	
	
	/* outputs */

	{ SSC_OUTPUT,       SSC_ARRAY,       "gh",                             "Global horizontal irradiance",                "W/m2",   "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "df",                             "Diffuse irradiance",                          "W/m2",   "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tamb",                           "Ambient temperature",                         "C",      "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",                           "Wind speed",                                  "m/s",    "",                        "Time series",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "sunup",                          "Sun up over horizon",                         "0/1",    "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "shad_beam_factor",               "Shading factor for beam radiation",           "",       "",                        "Time series",      "*",                       "",                                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tpoa",                           "Transmitted plane of array irradiance",       "W/m2",   "",                        "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "tcell",                          "Module temperature",                          "C",      "",                        "Time series",      "*",                       "",                          "" },	
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc",                             "DC array power",                              "W",     "",                         "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                             "AC inverter power",                           "W",     "",                         "Time series",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "energy",                         "Net system energy",                           "Wh",     "",                        "Time series",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",                  "Hourly energy",                               "kWh",    "",                        "Time series",      "*",                       "LENGTH=8760",                          "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa_monthly",                    "Plane of array irradiance",                   "kWh/m2",    "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solrad_monthly",                 "Daily average solar irradiance",              "kWh/m2/day","",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dc_monthly",                     "DC array output",                             "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "ac_monthly",                     "AC system output",                            "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",                 "Monthly energy",                              "kWh",       "",                     "Monthly",          "*",                       "LENGTH=12",                          "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "solrad_annual",                  "Daily average solar irradiance",              "kWh/m2/day",    "",              "Annual",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "ac_annual",                      "Annual AC system output",                     "kWh",         "",              "Annual",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                  "Annual energy",                               "kWh",           "",              "Annual",      "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",                "Capacity factor",                             "%",             "",              "Annual",        "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",                     "First year kWh/kW",                           "",              "",              "Annual",        "*",                       "",                          "" },

	{ SSC_OUTPUT,       SSC_STRING,      "location",                       "Location ID",                                 "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "city",                           "City",                                        "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_STRING,      "state",                          "State",                                       "",    "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lat",                            "Latitude",                                    "deg", "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "lon",                            "Longitude",                                   "deg", "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "tz",                             "Time zone",                                   "hr",  "",                        "Location",      "*",                       "",                          "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "elev",                           "Site elevation",                              "m",   "",                        "Location",      "*",                       "",                          "" },
	
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_use_lifetime_output",     "Use lifetime output",                         "0/1", "",                        "Miscellaneous", "*",                       "INTEGER",                   "" },

	var_info_invalid };

class cm_pvwattsv5 : public compute_module
{
public:
	
	cm_pvwattsv5()
	{
		add_var_info( _cm_vtab_pvwattsv5 );
		add_var_info( vtab_adjustment_factors );
	}

	double transmittance( double theta1, double n_material, double n_incoming, double k, double l, double *theta2_ret = 0 )
	{
		double theta2 = asind( n_incoming / n_material * sind(theta1 ) ); // % snell's law, assuming n_air = 1.0
		if ( theta2_ret != 0 ) *theta2_ret = theta2;

		// fresnel's equation for non-reflected unpolarized radiation as an average of perpendicular and parallel components
		double tr0 = 1 - 0.5 *
			( pow( sind(theta2-theta1), 2 )/pow( sind(theta2+theta1), 2)
			+ pow( tand(theta2-theta1), 2 )/pow( tand(theta2+theta1), 2 ) );

		return tr0; //* exp( -k * l / cosd(theta2) ); // Ignore Bouger's law since it has a truly tiny effect
	}
	
	double iam( double theta, bool ar_glass )
	{
		double normal = iam_nonorm( 1, ar_glass );
		double actual = iam_nonorm( theta, ar_glass );
		return actual/normal;	
	}

	double iam_nonorm( double theta, bool ar_glass )
	{
		double n_air = 1.0;

		double n_g = 1.526;
		double k_g = 4;
		double l_g = 0.002;

		double n_arc = 1.3;
		double k_arc = 4;
		double l_arc = l_g*0.01;  // assume 1/100th thickness of glass for AR coating

		if ( theta < 1 ) theta = 1;
		if ( theta > 89 ) theta = 89;

		if ( ar_glass )
		{
			double theta2 = 1;
			double tau_coating = transmittance( theta, n_arc, n_air, k_arc, l_arc, &theta2 );
			double tau_glass = transmittance( theta2, n_g, n_arc, k_g, l_g );
			return tau_coating*tau_glass;
		}
		else
		{
			return transmittance(theta, n_g, n_air, k_g, l_g );
		}
	}

	void exec( ) throw( general_error )
	{

		const char *file = as_string("solar_resource_file");

		weatherfile wf( file );
		if (!wf.ok()) throw exec_error("pvwattsv5", "failed to read local weather file: " + std::string(file));
										
		double dc_nameplate = as_double("system_capacity")*1000;
		double dc_ac_ratio = as_double("dc_ac_ratio");
		double ac_nameplate = dc_nameplate / dc_ac_ratio;
		double inv_eff_percent = as_double("inv_eff");
		
		double loss_percent = as_double("losses");        
		double tilt = as_double("tilt");
		double azimuth = as_double("azimuth");

		double gamma = 0;
		bool use_ar_glass = false;

		int module_type = as_integer("module_type");
		switch( module_type )
		{
		case 0: // standard module
			gamma = -0.0047; use_ar_glass = false; break;
		case 1: // premium module
			gamma = -0.0035; use_ar_glass = true; break;
		case 2: // thin film module
			gamma = -0.0020; use_ar_glass = false; break;
		}

		int track_mode =  0;
		double inoct = 45;
		int shade_mode_1x = 0; // self shaded
		
		int array_type = as_integer("array_type"); // 0, 1, 2, 3, 4		
		switch( array_type )
		{
		case 0: // fixed open rack
			track_mode = 0; inoct = 45; shade_mode_1x = 0; break;
		case 1: // fixed roof mount
			track_mode = 0; inoct = 49; shade_mode_1x = 0; break;
		case 2: // 1 axis self-shaded
			track_mode = 1; inoct = 45; shade_mode_1x = 0; break;
		case 3: // 1 axis backtracked
			track_mode = 1; inoct = 45; shade_mode_1x = 1; break;
		case 4: // 2 axis
			track_mode = 2; inoct = 45; shade_mode_1x = 0; break;
		case 5: // azimuth axis
			track_mode = 3; inoct = 45; shade_mode_1x = 0; break;
		}

		
		double gcr = 0.4;
		if ( track_mode == 1 && is_assigned("gcr") ) gcr = as_double("gcr");

		
		adjustment_factors haf( this );
		if ( !haf.setup() )
			throw exec_error("pvwattsv5", "failed to setup adjustment factors: " + haf.error() );
		
		// read all the shading input data and calculate the hourly factors for use subsequently
		shading_factor_calculator shad;
		if ( !shad.setup( this, "" ) )
			throw exec_error( "pvwattsv5", shad.get_error() );

		size_t nrec = wf.nrecords;
		size_t step_per_hour = nrec/8760;
		if ( step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != nrec )
			throw exec_error( "pvwattsv5", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec ) );
		
		
		/* allocate output arrays */		
		ssc_number_t *p_gh = allocate("gh", nrec);
		ssc_number_t *p_dn = allocate("dn", nrec);
		ssc_number_t *p_df = allocate("df", nrec);
		ssc_number_t *p_tamb = allocate("tamb", nrec);
		ssc_number_t *p_wspd = allocate("wspd", nrec);
		
		ssc_number_t *p_sunup = allocate("sunup", nrec);
		ssc_number_t *p_shad_beam = allocate("shad_beam_factor", nrec); // just for reporting output

		ssc_number_t *p_tcell = allocate("tcell", nrec);
		ssc_number_t *p_poa = allocate("poa", nrec);
		ssc_number_t *p_tpoa = allocate("tpoa", nrec);
		ssc_number_t *p_dc = allocate("dc", nrec);
		ssc_number_t *p_ac = allocate("ac", nrec);
		ssc_number_t *p_energy = allocate("energy", nrec);
		
		ssc_number_t *p_hourly_energy = allocate("hourly_energy", 8760);
				
		double ts_hour = 1.0/step_per_hour;
		pvwatts_celltemp tccalc( inoct+273.15, PVWATTS_HEIGHT, ts_hour );

		double annual_wh = 0; 
					
		size_t hour=0, idx=0;
		while( hour < 8760 )
		{
			
#define NSTATUS_UPDATES 50  // set this to the number of times a progress update should be issued for the simulation
			if ( hour % (8760/NSTATUS_UPDATES) == 0 )
			{
				float percent = 100.0f * ((float)hour+1) / ((float)8760);
				if ( !update( "calculating", percent , (float)hour ) )
					throw exec_error("pvwattsv5", "simulation canceled at hour " + util::to_string(hour+1.0) );
			}


			for( size_t jj=0;jj<step_per_hour;jj++)
			{
				if (!wf.read())
					throw exec_error("pvwattsv5", util::format("could not read data line %d of %d in weather file", (int)(idx+1), (int)nrec ));

				irrad irr;
				irr.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, ts_hour );
				irr.set_location( wf.lat, wf.lon, wf.tz );
				
				double alb = 0.2; // do not increase albedo if snow exists in TMY2
			
				if ( wf.type() == weatherfile::TMY3 
					|| wf.type() == weatherfile::WFCSV )
				{
					if ( wf.albedo >= 0 && wf.albedo < 1 )
						alb = wf.albedo;
				}

				irr.set_sky_model( 2, alb );
				irr.set_beam_diffuse( wf.dn, wf.df );
				irr.set_surface( track_mode, tilt, azimuth, 45.0, 
					shade_mode_1x == 1, // backtracking mode
					gcr );
			
				double ibeam, iskydiff, ignddiff;
				double solazi, solzen, solalt, aoi, stilt, sazi, rot, btd;
				int sunup;		

				p_gh[idx] = (ssc_number_t)wf.gh;
				p_dn[idx] = (ssc_number_t)wf.dn;
				p_df[idx] = (ssc_number_t)wf.df;
				p_tamb[idx] = (ssc_number_t)wf.tdry;
				p_wspd[idx] = (ssc_number_t)wf.wspd;			
				p_tcell[idx] = (ssc_number_t)wf.tdry;
	
				int code = irr.calc();

				if ( 0 != code )
					throw exec_error( "pvwattsv5", 
						util::format("failed to process irradiation on surface (code: %d) [y:%d m:%d d:%d h:%d]", 
							code, wf.year, wf.month, wf.day, wf.hour));

				irr.get_sun( &solazi, &solzen, &solalt, 0, 0, 0, &sunup, 0, 0, 0 );			
				p_sunup[idx] = (ssc_number_t)sunup;
				p_shad_beam[idx] = (ssc_number_t) shad.fbeam( hour, solalt, solazi );

				if (sunup > 0)
				{
					irr.get_angles( &aoi, &stilt, &sazi, &rot, &btd );
					irr.get_poa( &ibeam, &iskydiff, &ignddiff, 0, 0, 0);
				
					if ( sunup > 0 && track_mode == 1
						&& shade_mode_1x == 0 ) // selfshaded mode
					{	
						double shad1xf = shade_fraction_1x( solazi, solzen, tilt, azimuth, gcr, rot );					
						p_shad_beam[idx] *= (ssc_number_t)(1-shad1xf);

						if ( shade_mode_1x == 0 && iskydiff > 0 )
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
							else log( util::format("sky diffuse reduction factor invalid at time %d: fskydiff=%lg, stilt=%lg", (int)idx, Fskydiff, stilt), SSC_NOTICE, (float)idx );

							if ( Fgnddiff >= 0 && Fgnddiff <= 1 ) ignddiff *= Fgnddiff;
							else log( util::format("gnd diffuse reduction factor invalid at time %d: fgnddiff=%lg, stilt=%lg", (int)idx, Fgnddiff, stilt), SSC_NOTICE, (float)idx );
						}

					}

					// apply hourly shading factors to beam (if none enabled, factors are 1.0)
					ibeam *= p_shad_beam[idx];
				
					// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
					iskydiff *= shad.fdiff();
				
					double poa = ibeam + iskydiff +ignddiff;
				
					double wspd_corr = wf.wspd < 0 ? 0 : wf.wspd;
								
					// module cover
					double tpoa = poa - ( 1.0 - iam( aoi, use_ar_glass ) )*wf.dn*cosd(aoi);
					if( tpoa < 0.0 ) tpoa = 0.0;
			
					// cell temperature
					double pvt = tccalc( poa, wspd_corr, wf.tdry );

					// dc power output (Watts)
					double dc = dc_nameplate*(1.0+gamma*(pvt-25.0))*tpoa/1000.0;

					// dc losses
					dc = dc*(1-loss_percent/100);

					// inverter efficiency
					double etanom = inv_eff_percent/100.0;
					double etaref = 0.9637;
					double A =  -0.0162;
					double B = -0.0059;
					double C =  0.9858;
					double pdc0 = ac_nameplate/etanom;
					double plr = dc / pdc0;
					double ac = 0;
				
					if ( plr > 0 )
					{ // normal operation
						double eta = (A*plr + B/plr + C)*etanom/etaref;
						ac = dc*eta;
					}

					if ( ac > ac_nameplate ) // clipping
						ac = ac_nameplate;

					// make sure no negative AC values (no parasitic nighttime losses calculated)
					if ( ac < 0 ) ac = 0;
			
					p_poa[idx] = (ssc_number_t)poa; // W/m2
					p_tpoa[idx] = (ssc_number_t)tpoa;  // W/m2
					p_tcell[idx] = (ssc_number_t)pvt;
					p_dc[idx] = (ssc_number_t)dc; // power, Watts
					p_ac[idx] = (ssc_number_t)ac; // power, Watts
					p_energy[idx] = (ssc_number_t)( ac * ts_hour * haf(hour) ); // Wh energy, with adjustment factors applied

					// accumulate annual total energy for capacity factor calculation
					annual_wh += p_energy[idx];

					// accumulate hourly energy (kWh) (was initialized to zero when allocated)
					p_hourly_energy[hour] += p_energy[idx] * 0.001f;
				}
						
				idx++;
			}

			hour++;
		}

		accumulate_monthly( "dc", "dc_monthly", 0.001*ts_hour );
		accumulate_monthly( "ac", "ac_monthly", 0.001*ts_hour );
		accumulate_monthly( "hourly_energy", "monthly_energy" );
		
		ssc_number_t *poam = accumulate_monthly( "poa", "poa_monthly", 0.001*ts_hour ); // convert to energy
		ssc_number_t *solrad = allocate( "solrad_monthly", 12 );
		ssc_number_t solrad_ann = 0;
		for ( int m=0;m<12;m++ )
		{
			solrad[m] = poam[m]/util::nday[m];
			solrad_ann += solrad[m];
		}
		assign( "solrad_annual", var_data( solrad_ann/12 ) );

		accumulate_annual( "ac", "ac_annual", 0.001*ts_hour );
		accumulate_annual( "hourly_energy", "annual_energy" ); 

		assign("system_use_lifetime_output", 0);
		assign( "location", var_data( wf.location ) );
		assign( "city", var_data( wf.city ) );
		assign( "state", var_data( wf.state ) );
		assign( "lat", var_data( (ssc_number_t)wf.lat ) );
		assign( "lon", var_data( (ssc_number_t)wf.lon ) );
		assign( "tz", var_data( (ssc_number_t)wf.tz ) );
		assign( "elev", var_data( (ssc_number_t)wf.elev ) );
		

		// metric outputs moved to technology
		double kWhperkW = annual_wh / dc_nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	}
};

DEFINE_MODULE_ENTRY( pvwattsv5, "PVWatts V5 - integrated hourly weather reader and PV system simulator.", 3 )
