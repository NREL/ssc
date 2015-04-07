#include "core.h"

#include "csp_solver_core.h"
#include "csp_solver_221_222.h"

#include "lib_util.h"

#include "lib_weatherfile.h"
#include <algorithm>

#include "interpolation_routines.h"

#include "htf_props.h"

using namespace std;

static bool solarpilot_callback(simulation_info *siminfo, void *data);

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

#define pi 3.141592654
#define az_scale 6.283125908 
#define zen_scale 1.570781477 
#define eff_scale 0.7

enum{	//Parameters
		P_run_type,                              
		P_helio_width, 
		P_helio_height, 
		P_helio_optical_error, 
		P_helio_active_fraction, 
        P_dens_mirror,
		P_helio_reflectance, 
		P_rec_absorptance, 
		P_rec_height, 
		P_rec_aspect, 
		P_rec_hl_perm2, 
		P_q_design, 
		P_h_tower, 
		P_weather_file,
		P_land_bound_type, 
		P_land_max, 
		P_land_min, 
		P_land_bound_table, 
		P_land_bound_list, 
		P_p_start, 
		P_p_track, 
		P_hel_stow_deploy, 
		P_v_wind_max, 
		P_interp_nug, 
		P_interp_beta, 
		P_n_flux_x, 
		P_n_flux_y, 
		P_helio_positions, 
		P_helio_aim_points, 
		P_N_hel, 
		P_eta_map, 
		P_flux_positions, 
		P_flux_maps,
		P_c_atm_0,
		P_c_atm_1,
		P_c_atm_2,
		P_c_atm_3,
		P_n_facet_x,
		P_n_facet_y,
		P_cant_type,
		P_focus_type,
		P_n_flux_days,
		P_delta_flux_hrs,
		P_dni_des,
		P_land_area,

		O_pparasi,
		O_eta_field,
		O_flux_map,

		//N_MAX
		N_MAX};

const tcsvarinfo csp_solver_221_222_params[] = {
	{ TCS_PARAM,    TCS_NUMBER,   P_run_type,                "run_type",              "Run type",                                             "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_helio_width,             "helio_width",           "Heliostat width",                                      "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_helio_height,            "helio_height",          "Heliostat height",                                     "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_helio_optical_error,     "helio_optical_error",   "Heliostat optical error",                              "rad",    "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_helio_active_fraction,   "helio_active_fraction", "Heliostat active frac.",                               "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_dens_mirror,             "dens_mirror",           "Ratio of reflective area to profile",                  "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_helio_reflectance,       "helio_reflectance",     "Heliostat reflectance",                                "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_rec_absorptance,         "rec_absorptance",       "Receiver absorptance",                                 "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_rec_height,              "rec_height",            "Receiver height",                                      "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_rec_aspect,              "rec_aspect",            "Receiver aspect ratio",                                "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_rec_hl_perm2,            "rec_hl_perm2",          "Receiver design heatloss",                             "kW/m2",  "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_q_design,                "q_design",              "Field thermal power rating",                           "kW",     "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_h_tower,                 "h_tower",               "Tower height",                                         "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_STRING,   P_weather_file,            "weather_file",          "Weather file location",                                "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_land_bound_type,         "land_bound_type",       "Land boundary type",                                   "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_land_max,                "land_max",              "Land max boundary",                                    "- OR m", "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_land_min,                "land_min",              "Land min boundary",                                    "- OR m", "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_land_bound_table,        "land_bound_table",      "Land boundary table",                                  "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_ARRAY,    P_land_bound_list,         "land_bound_list",       "Boundary table listing",                               "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_p_start,                 "p_start",               "Heliostat startup energy",                             "kWe-hr", "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_p_track,                 "p_track",               "Heliostat tracking energy",                            "kWe",    "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_hel_stow_deploy,         "hel_stow_deploy",       "Stow/deploy elevation",                                "deg",    "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_v_wind_max,              "v_wind_max",            "Max. wind velocity",                                   "m/s",    "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_interp_nug,              "interp_nug",            "Interpolation nugget",                                 "-",      "",                              "", "0.0"       },
	{ TCS_PARAM,    TCS_NUMBER,   P_interp_beta,             "interp_beta",           "Interpolation beta coef.",                             "-",      "",                              "", "1.99"      },
	{ TCS_PARAM,    TCS_NUMBER,   P_n_flux_x,                "n_flux_x",              "Flux map X resolution",                                "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_n_flux_y,                "n_flux_y",              "Flux map Y resolution",                                "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_helio_positions,         "helio_positions",       "Heliostat position table",                             "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_helio_aim_points,        "helio_aim_points",      "Heliostat aim point table",                            "m",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_N_hel,                   "N_hel",                 "Number of heliostats",                                 "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_eta_map,                 "eta_map",               "Field efficiency array",                               "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_flux_positions,          "flux_positions",        "Flux map sun positions",                               "deg",    "",                              "", ""          },
	{ TCS_PARAM,    TCS_MATRIX,   P_flux_maps,               "flux_maps",             "Flux map intensities",                                 "-",      "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_c_atm_0,                 "c_atm_0",               "Attenuation coefficient 0",                            "",       "",                              "", "0.006789"  },
	{ TCS_PARAM,    TCS_NUMBER,   P_c_atm_0,                 "c_atm_1",               "Attenuation coefficient 1",                            "",       "",                              "", "0.1046"    },
	{ TCS_PARAM,    TCS_NUMBER,   P_c_atm_0,                 "c_atm_2",               "Attenuation coefficient 2",                            "",       "",                              "", "-0.0107"   },
	{ TCS_PARAM,    TCS_NUMBER,   P_c_atm_0,                 "c_atm_3",               "Attenuation coefficient 3",                            "",       "",                              "", "0.002845"  },
	{ TCS_PARAM,    TCS_NUMBER,   P_n_facet_x,               "n_facet_x",             "Number of heliostat facets - X",                       "",       "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_n_facet_y,               "n_facet_y",             "Number of heliostat facets - Y",                       "",       "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_cant_type,               "cant_type",             "Heliostat cant method",                                "",       "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_focus_type,              "focus_type",            "Heliostat focus method",                               "",       "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_n_flux_days,             "n_flux_days",           "No. days in flux map lookup",                          "",       "",                              "", "8"         },
	{ TCS_PARAM,    TCS_NUMBER,   P_delta_flux_hrs,          "delta_flux_hrs",        "Hourly frequency in flux map lookup",                  "hrs",    "",                              "", "1"         },
	{ TCS_PARAM,    TCS_NUMBER,   P_dni_des,                 "dni_des",               "Design-point DNI",                                     "W/m2",   "",                              "", ""          },
	{ TCS_PARAM,    TCS_NUMBER,   P_land_area,               "land_area",             "CALCULATED land area",                                 "acre",   "",                              "", ""          },


	{ TCS_OUTPUT,   TCS_NUMBER,   O_pparasi,                 "pparasi",               "Parasitic tracking/startup power",                     "MWe",    "",                              "", ""          },
    { TCS_OUTPUT,   TCS_NUMBER,   O_eta_field,               "eta_field",             "Total field efficiency",                               "",       "",                              "", ""          },
    { TCS_OUTPUT,   TCS_MATRIX,   O_flux_map,                "flux_map",              "Receiver flux map",                                    "",       "n_flux_x cols x n_flux_y rows", "", ""          },



	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	}
};

C_csp_mspt_221_222::C_csp_mspt_221_222()
{
	set_params_and_size_vector(csp_solver_221_222_params);

	run_type = 0;
	helio_width = std::numeric_limits<double>::quiet_NaN();
	helio_height = std::numeric_limits<double>::quiet_NaN();
	helio_optical_error = std::numeric_limits<double>::quiet_NaN();
	helio_active_fraction = std::numeric_limits<double>::quiet_NaN();
	dens_mirror = std::numeric_limits<double>::quiet_NaN();
	helio_reflectance = std::numeric_limits<double>::quiet_NaN();
	rec_absorptance = std::numeric_limits<double>::quiet_NaN();
	rec_height = std::numeric_limits<double>::quiet_NaN();
	rec_aspect = std::numeric_limits<double>::quiet_NaN();
	rec_hl_perm2 = std::numeric_limits<double>::quiet_NaN();
	q_design = std::numeric_limits<double>::quiet_NaN();
	h_tower = std::numeric_limits<double>::quiet_NaN();
	land_bound_type = 0;
	land_max = std::numeric_limits<double>::quiet_NaN();
	land_min = std::numeric_limits<double>::quiet_NaN();
	land_bound_table = NULL;
	land_bound_list = NULL;
	p_start = std::numeric_limits<double>::quiet_NaN();
	p_track = std::numeric_limits<double>::quiet_NaN();
	hel_stow_deploy = std::numeric_limits<double>::quiet_NaN();
	v_wind_max = std::numeric_limits<double>::quiet_NaN();
	interp_nug = std::numeric_limits<double>::quiet_NaN();
	interp_beta = std::numeric_limits<double>::quiet_NaN();
	helio_positions = NULL;
	helio_aim_points = NULL;
	N_hel = 0;
	pos_dim = 0;
	eta_map = NULL;
	n_flux_x = 0;
	n_flux_y = 0;
	flux_positions = NULL;
	flux_maps = NULL;
	flux_map = NULL;
	n_facet_x = 0;
	n_facet_y = 0;
	cant_type = 0;
	focus_type = 0;
	n_flux_days = 0;
	delta_flux_hrs = 0;

	eta_prev = std::numeric_limits<double>::quiet_NaN();
	v_wind_prev = std::numeric_limits<double>::quiet_NaN();
	c_atm_0 = std::numeric_limits<double>::quiet_NaN();
	c_atm_1 = std::numeric_limits<double>::quiet_NaN();
	c_atm_2 = std::numeric_limits<double>::quiet_NaN();
	c_atm_3 = std::numeric_limits<double>::quiet_NaN();
	dni_des = std::numeric_limits<double>::quiet_NaN();

	field_efficiency_table = 0;
}

C_csp_mspt_221_222::~C_csp_mspt_221_222()
{
	if( field_efficiency_table != 0 )
		delete field_efficiency_table;
}

void C_csp_mspt_221_222::init()
{
	//Read in parameters
	int nrows1, ncols1;
	int nrows2;
	int nrows4, ncols4;
	int nrows5, ncols5;
	int nfluxpos, nfposdim;
	int nfluxmap, nfluxcol;

	run_type = (int)value(P_run_type);

	//Read in only those parameters that are relevant to the run scheme
	switch( run_type )
	{
	case C_csp_mspt_221_222::RUN_TYPE::AUTO:
	case C_csp_mspt_221_222::RUN_TYPE::USER_FIELD:
		helio_width = value(P_helio_width);
		helio_height = value(P_helio_height);
		helio_optical_error = value(P_helio_optical_error);
		helio_active_fraction = value(P_helio_active_fraction);
		dens_mirror = value(P_dens_mirror);
		helio_reflectance = value(P_helio_reflectance);
		rec_absorptance = value(P_rec_absorptance);
		rec_height = value(P_rec_height);
		rec_aspect = value(P_rec_aspect);
		rec_hl_perm2 = value(P_rec_hl_perm2);
		q_design = value(P_q_design);
		h_tower = value(P_h_tower);
		weather_file = value_str(P_weather_file);
		land_bound_type = (int)value(P_land_bound_type);
		land_max = value(P_land_max);
		land_min = value(P_land_min);
		land_bound_table = value(P_land_bound_table, &nrows1, &ncols1);
		land_bound_list = value(P_land_bound_list, &nrows2);
		p_start = value(P_p_start);
		p_track = value(P_p_track);
		hel_stow_deploy = value(P_hel_stow_deploy)*pi / 180.;
		v_wind_max = value(P_v_wind_max);
		interp_nug = value(P_interp_nug);
		interp_beta = value(P_interp_beta);
		n_flux_x = (int)value(P_n_flux_x);
		n_flux_y = (int)value(P_n_flux_y);
		c_atm_0 = value(P_c_atm_0);
		c_atm_1 = value(P_c_atm_1);
		c_atm_2 = value(P_c_atm_2);
		c_atm_3 = value(P_c_atm_3);
		n_facet_x = (int)value(P_n_facet_x);
		n_facet_y = (int)value(P_n_facet_y);
		cant_type = (int)value(P_cant_type);
		focus_type = (int)value(P_focus_type);
		n_flux_days = (int)value(P_n_flux_days);
		delta_flux_hrs = (int)value(P_delta_flux_hrs);
		dni_des = value(P_dni_des);

		pos_dim = 2;	//initiaize with 2 dimensions (x,y) on helio positions
		if( run_type != C_csp_mspt_221_222::RUN_TYPE::USER_FIELD ) break;

		helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);

		break;
	case C_csp_mspt_221_222::RUN_TYPE::USER_DATA:

		h_tower = value(P_h_tower);
		land_bound_type = (int)value(P_land_bound_type);
		land_max = value(P_land_max);
		land_min = value(P_land_min);
		land_bound_table = value(P_land_bound_table, &nrows1, &ncols1);
		land_bound_list = value(P_land_bound_list, &nrows2);
		p_start = value(P_p_start);
		p_track = value(P_p_track);
		hel_stow_deploy = value(P_hel_stow_deploy)*pi / 180.;
		v_wind_max = value(P_v_wind_max);
		interp_nug = value(P_interp_nug);
		interp_beta = value(P_interp_beta);
		helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
		//N_hel = (int)value(P_N_hel);
		eta_map = value(P_eta_map, &nrows5, &ncols5);
		n_flux_x = (int)value(P_n_flux_x);
		n_flux_y = (int)value(P_n_flux_y);
		/*int nfluxpos, nfposdim;
		int nfluxmap, nfluxcol;*/
		flux_positions = value(P_flux_positions, &nfluxpos, &nfposdim);
		flux_maps = value(P_flux_maps, &nfluxmap, &nfluxcol);
		c_atm_0 = value(P_c_atm_0);
		c_atm_1 = value(P_c_atm_1);
		c_atm_2 = value(P_c_atm_2);
		c_atm_3 = value(P_c_atm_3);
		n_facet_x = (int)value(P_n_facet_x);
		n_facet_y = (int)value(P_n_facet_y);
		cant_type = (int)value(P_cant_type);
		focus_type = (int)value(P_focus_type);
		n_flux_days = (int)value(P_n_flux_days);
		delta_flux_hrs = (int)value(P_delta_flux_hrs);
		dni_des = value(P_dni_des);

		//check that flux maps match dimensions
		if( nfluxmap % nfluxpos != 0 )
		{
			char tstr[300];

			sprintf(tstr, "The number of flux maps provided does not match the number of flux map sun positions provided. Please "
				"ensure that the dimensionality of each flux map is consistent and that one sun position is provided for "
				"each flux map. (Sun pos. = %d, mismatch lines = %d)", nfluxpos, nfluxmap % nfluxpos);

			throw exec_error("SolarPILOT initializiation", tstr);
		}

		//copy the flux positions over to the local member
		m_flux_positions.resize(nfluxpos, VectDoub(nfposdim));
		for( int i = 0; i<nfluxpos; i++ )
		for( int j = 0; j<nfposdim; j++ )
			m_flux_positions.at(i).at(j) = flux_positions[i * 2 + j];

		break;
	default:
		break;
	}

	MatDoub sunpos;
	vector<double> effs;
	


	//do initial runs of SolarPILOT and/or set up tables
	switch (run_type)
	{
	case C_csp_mspt_221_222::RUN_TYPE::AUTO:
	case C_csp_mspt_221_222::RUN_TYPE::USER_FIELD:
	{
		AutoPilot_S sapi;

		sp_optimize opt;
		sp_ambient amb;
		sp_cost cost;
		sp_heliostats helios;
		sp_receivers recs;
		sp_layout layout;
	
		var_set V;
		ioutil::parseDefinitionArray(V);
	
		// define stuff and load default values
		opt.LoadDefaults(V);
		amb.LoadDefaults(V);
		cost.LoadDefaults(V);
		helios.resize(1);
		helios.front().LoadDefaults(V);
		recs.resize(1);
		recs.front().LoadDefaults(V);
		layout.LoadDefaults(V);

		helios.front().width = helio_width;
		helios.front().height = helio_height;
		helios.front().optical_error = helio_optical_error;
		helios.front().active_fraction = helio_active_fraction * dens_mirror;   //availability * mirror area fraction
		helios.front().reflectance = helio_reflectance;
		int cmap[5];
		cmap[0] = sp_heliostat::CANT_TYPE::FLAT;
		cmap[1] = sp_heliostat::CANT_TYPE::AT_SLANT;
		cmap[2] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
		cmap[3] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
		cmap[4] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
		helios.front().cant_type = cmap[ cant_type ];
		if( cant_type == 2 ){
			helios.front().cant_settings.point_day = 81;  //spring equinox
			helios.front().cant_settings.point_hour = 12.;
		}
		else if( cant_type == 3 ){
			helios.front().cant_settings.point_day = 172;  //Summer solstice
			helios.front().cant_settings.point_hour = 12.;
		}
		else if( cant_type == 4){
			helios.front().cant_settings.point_day = 355;  //Winter solstice
			helios.front().cant_settings.point_hour = 12.;
		}

		int fmap[2];
		fmap[0] = sp_heliostat::FOCUS_TYPE::FLAT;
		fmap[1] = sp_heliostat::FOCUS_TYPE::AT_SLANT;
		helios.front().focus_type = fmap[ focus_type ];

		recs.front().absorptance = rec_absorptance;
		recs.front().height = rec_height;
		recs.front().aspect = rec_aspect;
		recs.front().q_hl_perm2 = rec_hl_perm2;
			
		layout.q_design = q_design;
		layout.dni_design = dni_des;
		layout.land_max = land_max;
		layout.land_min = land_min;
		layout.h_tower = h_tower;

		//set up the weather data for simulation
		const char *wffile = weather_file.c_str();
		
		//if ( !wffile ) message(TCS_WARNING,  "solarpilot: no weather file specified" );
		
		weatherfile wf( wffile );
		
		//if ( !wf.ok() || wf.type() == weatherfile::INVALID ) message( TCS_WARNING, "solarpilot: could not open weather file or invalid weather file format");


		amb.site_latitude = wf.lat;
		amb.site_longitude = wf.lon;
		amb.site_time_zone = wf.tz;
		amb.atten_model = sp_ambient::ATTEN_MODEL::USER_DEFINED;
		amb.user_atten_coefs.clear();
		amb.user_atten_coefs.push_back(c_atm_0);
		amb.user_atten_coefs.push_back(c_atm_1);
		amb.user_atten_coefs.push_back(c_atm_2);
		amb.user_atten_coefs.push_back(c_atm_3);

		if( run_type == C_csp_mspt_221_222::RUN_TYPE::AUTO )
		{
			/* 
			Generate the heliostat field layout using the settings provided by the user				
			*/
			vector<string> wfdata;
			wfdata.reserve( 8760 );
			char buf[1024];
			for( int i=0;i<8760;i++ )
			{
				if( !wf.read() )
				{
					string msg = "solarpilot: could not read data line " + util::to_string(i+1) + " of 8760 in weather file";
					log(msg);
					//message(TCS_WARNING, msg.c_str());
				}

				mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
				wfdata.push_back( std::string(buf) );
			}

			sapi.SetDetailCallback( solarpilot_callback, (void*)this);
			sapi.SetSummaryCallbackStatus(false);

			sapi.GenerateDesignPointSimulations( amb, V, wfdata );
	
			sapi.Setup(amb, cost, layout, helios, recs);

			sapi.CreateLayout();


			//Copy the heliostat field positions into the 'helio_positions' data structure
			N_hel = (int)layout.heliostat_positions.size();
            string msg = "Auto-generated field: Number of heliostats " + util::to_string(N_hel);
            
			log(msg);
			//message(TCS_NOTICE, msg.c_str());
			
			helio_positions = allocate(P_helio_positions, N_hel, pos_dim);
			
			for( int i=0; i<N_hel; i++)
			{
				tcsmatrix_index(P_helio_positions, i, 0, layout.heliostat_positions.at(i).location.x);
				tcsmatrix_index(P_helio_positions, i, 1, layout.heliostat_positions.at(i).location.y);
				if( pos_dim == 3 )
					tcsmatrix_index(P_helio_positions, i, 2, layout.heliostat_positions.at(i).location.z);
			}
				
			//update the callbacks
			sapi.SetDetailCallbackStatus(false);
				
		}
		else{

			/* 
			Load in the heliostat field positions that are provided by the user.
			*/
			layout.heliostat_positions.clear();
			layout.heliostat_positions.resize(N_hel);
				
			for( int i=0; i<N_hel; i++){

				layout.heliostat_positions.at(i).location.x = tcsmatrix_index(P_helio_positions, i, 0);
				layout.heliostat_positions.at(i).location.y = tcsmatrix_index(P_helio_positions, i, 1);
				if(pos_dim==3)
					layout.heliostat_positions.at(i).location.z = tcsmatrix_index(P_helio_positions, i, 2);
					
			}

            sapi.Setup(amb, cost, layout, helios, recs);
								
		}
        //land area update
		value(P_land_area, layout.land_area);
        //number of heliostats
		value(P_N_hel, (double)N_hel);

		sapi.SetSummaryCallbackStatus(true);
		sapi.SetSummaryCallback( solarpilot_callback, (void*)this);

		//set up flux map resolution
		fluxtab.is_user_spacing = true;
		fluxtab.n_flux_days = n_flux_days;
		fluxtab.delta_flux_hrs = delta_flux_hrs;

		//run the flux maps
		if(! sapi.CalculateFluxMaps(fluxtab, n_flux_x, n_flux_y, true) )
		{
			throw exec_error("SolarPILOT", "Simulation cancelled during fluxmap preparation");
			//message(TCS_ERROR, "Simulation cancelled during fluxmap preparation");
            //return -1;
        }

		//collect efficiencies
		sunpos.clear();
		effs.clear();
		int npos = (int)fluxtab.azimuths.size();
		sunpos.reserve(npos);
		effs.reserve(npos);

        eta_map = allocate( P_eta_map, npos, 3 );
        m_flux_positions.resize(npos, VectDoub(2) );

		for(int i=0; i<npos; i++){
			sunpos.push_back( vector<double>(2, 0.) );

			sunpos.back().at(0) = fluxtab.azimuths.at(i) / az_scale;
			sunpos.back().at(1) = fluxtab.zeniths.at(i) / zen_scale;
			effs.push_back( fluxtab.efficiency.at(i) / eff_scale );

            //fill the parameter matrix to return this data to calling program
            //also fill the flux sun positions matrix
			double check1    = m_flux_positions.at(i).at(0) = fluxtab.azimuths.at(i)*180. / pi;
			double check2    = m_flux_positions.at(i).at(1) = fluxtab.zeniths.at(i)*180. / pi;
			double check3    = fluxtab.efficiency.at(i);

            eta_map[i*3    ] = m_flux_positions.at(i).at(0) = fluxtab.azimuths.at(i)*180./pi;
            eta_map[i*3 + 1] = m_flux_positions.at(i).at(1) = fluxtab.zeniths.at(i)*180./pi;
            eta_map[i*3 + 2] = fluxtab.efficiency.at(i);

		}


		//collect flux's
		flux_maps = allocate(P_flux_maps, n_flux_y * npos, n_flux_x);

		block_t<double> *f = &fluxtab.flux_surfaces.front().flux_data;

		int nfl = f->nlayers();

		for( int i = 0; i<nfl; i++ ){
			for( int j = 0; j<n_flux_y; j++ ){
				for( int k = 0; k<n_flux_x; k++ ){
					tcsmatrix_index(P_flux_maps, i*n_flux_y + j, k, f->at(j, k, i));
				}
			}
		}

		break;
	}
	case C_csp_mspt_221_222::RUN_TYPE::USER_DATA:
	{

		int nrows, ncols;
		double *p_map = value( P_eta_map, &nrows, &ncols);
		
		if(ncols != 3)
		{
			char tstr[300];
			sprintf(tstr, "The heliostat field efficiency file is not formatted correctly. Type expects 3 columns", "(zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);

			throw exec_error("SolarPILOT", tstr);

			//message(TCS_ERROR,  "The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
			//	" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);
			//return -1;
		}
		
		//read the data from the array into the local storage arrays
		sunpos.resize(nrows, VectDoub(2));
		effs.resize(nrows);
		for(int i=0; i<nrows; i++){
			sunpos.at(i).at(0) = tcsmatrix_index(P_eta_map, i, 0) / az_scale * pi / 180.;
			sunpos.at(i).at(1) = tcsmatrix_index(P_eta_map, i, 1) / zen_scale * pi / 180.;
			effs.at(i) = tcsmatrix_index(P_eta_map, i, 2) / eff_scale;
		}

        break;
	}
	default:
		break;
	}

    //size the output
	flux_map = allocate( O_flux_map, n_flux_y, n_flux_x );

	//report back the flux positions used
	int nflux = (int)m_flux_positions.size();
	flux_positions = allocate(P_flux_positions, nflux, 2);
	for( int i = 0; i<nflux; i++ ){
		flux_positions[i * 2] = m_flux_positions.at(i).at(0);
		flux_positions[i * 2 + 1] = m_flux_positions.at(i).at(1);
	}

	/*
	------------------------------------------------------------------------------
	Create the regression fit on the efficiency map
	------------------------------------------------------------------------------
	*/

	//collect nug and beta
	interp_beta = value(P_interp_beta);
	interp_nug = value(P_interp_nug);

	//Create the field efficiency table
	Powvargram vgram(sunpos, effs, interp_beta, interp_nug);
	field_efficiency_table = new GaussMarkov(sunpos, effs, vgram);

	//test how well the fit matches the data
	double err_fit = 0.;
	int npoints = (int)sunpos.size();
	for( int i = 0; i<npoints; i++ ){
		double zref = effs.at(i);
		double zfit = field_efficiency_table->interp(sunpos.at(i));
		double dz = zref - zfit;
		err_fit += dz * dz;
	}
	err_fit = sqrt(err_fit);
	if( err_fit > 0.01 )
	{
		char tstr[300];

		sprintf(tstr, "The heliostat field interpolation function fit is poor! (err_fit=%f RMS) %d", err_fit);
		
		log(tstr);
		//message(TCS_WARNING, "The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);
	}

	// Initialize stored variables
	eta_prev = 0.0;
	v_wind_prev = 0.0;

	return;
}

int C_csp_mspt_221_222::relay_message(string &msg, double percent)
{
	double time = 1.234567;
	return update(msg, percent, time) ? 0 : -1;
}

static bool solarpilot_callback(simulation_info *siminfo, void *data)
{
	C_csp_mspt_221_222 *cm = static_cast<C_csp_mspt_221_222*>(data);
	if( !cm )
		return false;

	float simprogress = (float)siminfo->getCurrentSimulation() / (float)(max(siminfo->getTotalSimulationCount(), 1));
	
	return cm->relay_message(*siminfo->getSimulationNotices(), simprogress*100.0f) == 0;
}