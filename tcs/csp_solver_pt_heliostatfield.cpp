#include "csp_solver_pt_heliostatfield.h"
#include "sam_csp_util.h"
#include "csp_solver_core.h"

#include "interpolation_routines.h"
#include "AutoPilot_API.h"
#include "IOUtil.h"
#include "sort_method.h"

#include "lib_weatherfile.h"

#define az_scale 6.283125908 
#define zen_scale 1.570781477 
#define eff_scale 0.7

C_pt_heliostatfield::C_pt_heliostatfield()
{
	m_p_start = m_p_track = m_hel_stow_deploy = m_v_wind_max =
		m_eta_prev = m_v_wind_prev = m_v_wind_current = std::numeric_limits<double>::quiet_NaN();

	m_n_flux_x = m_n_flux_y = m_N_hel = -1;

	field_efficiency_table = 0;

	m_cdata = 0;		// = NULL
	mf_callback = 0;	// = NULL

	m_ncall = -1;
}

C_pt_heliostatfield::~C_pt_heliostatfield()
{
	if( field_efficiency_table != 0 )
		delete field_efficiency_table;
}

//void C_pt_heliostatfield::init(bool(*callback)(simulation_info* siminfo, void *data), void *cdata)
void C_pt_heliostatfield::init()
{
	//Read in parameters
	int nrows1, ncols1;
	int nrows2;
	int nrows4, ncols4;
	int nrows5, ncols5;
	int nfluxpos, nfposdim;
	int nfluxmap, nfluxcol;

	// Declare and initialize variables that are only used in initial call
	std::string weather_file;

	double helio_width = std::numeric_limits<double>::quiet_NaN();
	double helio_height = std::numeric_limits<double>::quiet_NaN();
	double helio_optical_error = std::numeric_limits<double>::quiet_NaN();
	double helio_active_fraction = std::numeric_limits<double>::quiet_NaN();
	double dens_mirror = std::numeric_limits<double>::quiet_NaN();
	double helio_reflectance = std::numeric_limits<double>::quiet_NaN();
	double rec_absorptance = std::numeric_limits<double>::quiet_NaN();
	double rec_height = std::numeric_limits<double>::quiet_NaN();
	double rec_aspect = std::numeric_limits<double>::quiet_NaN();
	double rec_hl_perm2 = std::numeric_limits<double>::quiet_NaN();
	double q_design = std::numeric_limits<double>::quiet_NaN();
	double h_tower = std::numeric_limits<double>::quiet_NaN();
	int land_bound_type = 0;
	double land_max = std::numeric_limits<double>::quiet_NaN();
	double land_min = std::numeric_limits<double>::quiet_NaN();
	double interp_nug = std::numeric_limits<double>::quiet_NaN();
	double interp_beta = std::numeric_limits<double>::quiet_NaN();

	double c_atm_0 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_1 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_2 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_3 = std::numeric_limits<double>::quiet_NaN();

	int n_facet_x = 0;
	int n_facet_y = 0;

	int cant_type = 0;
	int focus_type = 0;

	int n_flux_days = 0;
	int delta_flux_hrs = 0;

	double dni_des = std::numeric_limits<double>::quiet_NaN();

	//double *helio_positions = NULL;
	util::matrix_t<double> helio_positions;	
	//double *eta_map = NULL;
	util::matrix_t<double> eta_map;
	//double *flux_maps = NULL;
	util::matrix_t<double> flux_maps;
	//double *land_bound_table = NULL;
	util::matrix_t<double> land_bound_table;
	//double *land_bound_list = NULL;
	util::matrix_t<double> land_bound_list;
	//double *helio_aim_points = NULL;
	util::matrix_t<double> helio_aim_points;
	//double *flux_positions = NULL;
	util::matrix_t<double> flux_positions;
	
	int pos_dim = 0;

	//double *flux_map = NULL;

	int run_type = ms_params.m_run_type;

	//Read in only those parameters that are relevant to the run scheme
	switch( run_type )
	{
	case RUN_TYPE::AUTO:
	case RUN_TYPE::USER_FIELD:
		helio_width = ms_params.m_helio_width;
		helio_height = ms_params.m_helio_height;
		helio_optical_error = ms_params.m_helio_optical_error;
		helio_active_fraction = ms_params.m_helio_active_fraction;
		dens_mirror = ms_params.m_dens_mirror;
		helio_reflectance = ms_params.m_helio_reflectance;
		rec_absorptance = ms_params.m_rec_absorptance;
		rec_height = ms_params.m_rec_height;
		rec_aspect = ms_params.m_rec_aspect;
		rec_hl_perm2 = ms_params.m_rec_hl_perm2;
		q_design = ms_params.m_q_design;
		h_tower = ms_params.m_h_tower;
		weather_file = ms_params.m_weather_file;
		land_bound_type = ms_params.m_land_bound_type;
		land_max = ms_params.m_land_max;
		land_min = ms_params.m_land_min;

		land_bound_table = ms_params.m_land_bound_table;
		nrows1 = land_bound_table.nrows();
		ncols1 = land_bound_table.ncols();
		//nrows1 = ms_params.m_nrows_land_bound_table;
		//ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);

		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = land_bound_list.nrows();
		//nrows2 = ms_params.m_nrows_land_bound_list;
		//land_bound_list = value(P_land_bound_list, &nrows2);

		m_p_start = ms_params.m_p_start;
		m_p_track = ms_params.m_p_track;
		m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;
		m_v_wind_max = ms_params.m_v_wind_max;
		
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;
		
		m_n_flux_x = ms_params.m_n_flux_x;
		m_n_flux_y = ms_params.m_n_flux_y;
		
		c_atm_0 = ms_params.m_c_atm_0;
		c_atm_1 = ms_params.m_c_atm_1;
		c_atm_2 = ms_params.m_c_atm_2;
		c_atm_3 = ms_params.m_c_atm_3;
		n_facet_x = ms_params.m_n_facet_x;
		n_facet_y = ms_params.m_n_facet_y;
		cant_type = ms_params.m_cant_type;
		focus_type = ms_params.m_focus_type;
		n_flux_days = ms_params.m_n_flux_days;
		delta_flux_hrs = ms_params.m_delta_flux_hrs;
		dni_des = ms_params.m_dni_des;

		pos_dim = 2;	//initiaize with 2 dimensions (x,y) on helio positions
		if( run_type != RUN_TYPE::USER_FIELD ) break;

		helio_positions = ms_params.m_helio_positions;
		m_N_hel = helio_positions.nrows();
		pos_dim = helio_positions.ncols();		
		//m_N_hel = ms_params.m_N_hel;
		//pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = helio_aim_points.nrows();
		ncols4 = helio_aim_points.ncols();
		//nrows4 = ms_params.m_nrows_helio_aim_points;
		//ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);

		break;
	case RUN_TYPE::USER_DATA:

		h_tower = ms_params.m_h_tower;
		land_bound_type = ms_params.m_land_bound_type;
		land_max = ms_params.m_land_max;
		land_min = ms_params.m_land_min;
		
		land_bound_table = ms_params.m_land_bound_table;
		nrows1 = land_bound_table.nrows();
		ncols1 = land_bound_table.ncols();
		//nrows1 = ms_params.m_nrows_land_bound_table;
		//ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);
		
		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = land_bound_list.nrows();
		//nrows2 = ms_params.m_nrows_land_bound_list;
		//land_bound_list = value(P_land_bound_list, &nrows2);
		
		m_p_start = ms_params.m_p_start;
		m_p_track = ms_params.m_p_track;
		m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;
		m_v_wind_max = ms_params.m_v_wind_max;
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;

		helio_positions = ms_params.m_helio_positions;
		m_N_hel = helio_positions.nrows();
		pos_dim = helio_positions.ncols();
		//m_N_hel = ms_params.m_N_hel;
		//pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = helio_aim_points.nrows();
		ncols4 = helio_aim_points.ncols();
		//nrows4 = ms_params.m_nrows_helio_aim_points;
		//ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
		
		eta_map = ms_params.m_eta_map;
		nrows5 = eta_map.nrows();
		ncols5 = eta_map.ncols();
		//nrows5 = ms_params.m_nrows_eta_map;
		//ncols5 = ms_params.m_ncols_eta_map;
		//eta_map = value(P_eta_map, &nrows5, &ncols5);

		m_n_flux_x = ms_params.m_n_flux_x;
		m_n_flux_y = ms_params.m_n_flux_y;
		
		flux_positions = ms_params.m_flux_positions;
		nfluxpos = flux_positions.nrows();
		nfposdim = flux_positions.ncols();
		//nfluxpos = ms_params.m_nfluxpos;
		//nfposdim = ms_params.m_nfposdim;
		//flux_positions = value(P_flux_positions, &nfluxpos, &nfposdim);

		flux_maps = ms_params.m_flux_maps;
		nfluxmap = flux_maps.nrows();
		nfluxcol = flux_maps.ncols();
		//nfluxmap = ms_params.m_nfluxmap;
		//nfluxcol = ms_params.m_nfluxcol;
		//flux_maps = value(P_flux_maps, &nfluxmap, &nfluxcol);

		c_atm_0 = ms_params.m_c_atm_0;
		c_atm_1 = ms_params.m_c_atm_1;
		c_atm_2 = ms_params.m_c_atm_2;
		c_atm_3 = ms_params.m_c_atm_3;
		n_facet_x = ms_params.m_n_facet_x;
		n_facet_y = ms_params.m_n_facet_y;
		cant_type = ms_params.m_cant_type;
		focus_type = ms_params.m_focus_type;
		n_flux_days = ms_params.m_n_flux_days;
		delta_flux_hrs = ms_params.m_delta_flux_hrs;
		dni_des = ms_params.m_dni_des;

		//check that flux maps match dimensions
		if( nfluxmap % nfluxpos != 0 )
		{
			error_msg = util::format("The number of flux maps provided does not match the number of flux map sun positions provided. Please "
				"ensure that the dimensionality of each flux map is consistent and that one sun position is provided for "
				"each flux map. (Sun pos. = %d, mismatch lines = %d)", nfluxpos, nfluxmap % nfluxpos);
			throw(C_csp_exception(error_msg, "heliostat field initialization"));
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
		case RUN_TYPE::AUTO:
		case RUN_TYPE::USER_FIELD:
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
			if( !wffile )
			{
				mc_csp_messages.add_message(C_csp_messages::WARNING, "solarpilot: could not open weather file or invalid weather file format");
				//message(TCS_WARNING, "solarpilot: no weather file specified");
			}
			weatherfile wf( wffile );
			if( !wf.ok() || wf.type() == weatherfile::INVALID )
			{
				mc_csp_messages.add_message(C_csp_messages::WARNING, "solarpilot: could not open weather file or invalid weather file format");
				//message(TCS_WARNING, "solarpilot: could not open weather file or invalid weather file format");
			}


			amb.site_latitude = wf.lat;
			amb.site_longitude = wf.lon;
			amb.site_time_zone = wf.tz;
			amb.atten_model = sp_ambient::ATTEN_MODEL::USER_DEFINED;
			amb.user_atten_coefs.clear();
			amb.user_atten_coefs.push_back(c_atm_0);
			amb.user_atten_coefs.push_back(c_atm_1);
			amb.user_atten_coefs.push_back(c_atm_2);
			amb.user_atten_coefs.push_back(c_atm_3);

			if(run_type == RUN_TYPE::AUTO)
			{
				/* 
				Generate the heliostat field layout using the settings provided by the user				
				*/
				vector<string> wfdata;
				wfdata.reserve( 8760 );
				//char buf[1024];
				for( int i=0;i<8760;i++ )
				{
					if( !wf.read() )
					{
						error_msg = "solarpilot: could not read data line " + util::to_string(i+1) + " of 8760 in weather file";
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
						//message(TCS_WARNING, msg.c_str());
					}

					error_msg = util::format("%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres / 1000., wf.wspd);
					wfdata.push_back(error_msg);
					//mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
					//wfdata.push_back( std::string(buf) );
				}

				if( mf_callback && m_cdata )
				{
					sapi.SetSummaryCallback(mf_callback, m_cdata);
				}
				sapi.SetSummaryCallbackStatus(false);



				sapi.GenerateDesignPointSimulations( amb, V, wfdata );
	
				sapi.Setup(amb, cost, layout, helios, recs);

				sapi.CreateLayout();


				//Copy the heliostat field positions into the 'helio_positions' data structure
				m_N_hel = (int)layout.heliostat_positions.size();
                string msg = "Auto-generated field: Number of heliostats " + util::to_string(m_N_hel);
				mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);		//message(TCS_NOTICE, msg.c_str());
				//helio_positions = allocate(P_helio_positions, m_N_hel, pos_dim);
				ms_params.m_helio_positions.resize(m_N_hel, pos_dim);
				for( int i=0; i<m_N_hel; i++)
				{
					ms_params.m_helio_positions(i,0) = layout.heliostat_positions.at(i).location.x;
					ms_params.m_helio_positions(i,1) = layout.heliostat_positions.at(i).location.y;
					if(pos_dim==3)
						ms_params.m_helio_positions(i, 2) = layout.heliostat_positions.at(i).location.z;
					//TCS_MATRIX_INDEX( var(P_helio_positions), i, 0 ) = layout.heliostat_positions.at(i).location.x;
					//TCS_MATRIX_INDEX( var(P_helio_positions), i, 1 ) = layout.heliostat_positions.at(i).location.y;
					//if(pos_dim==3)
					//	TCS_MATRIX_INDEX( var(P_helio_positions), i, 2) = layout.heliostat_positions.at(i).location.z;
				}
				
				//update the callbacks
				sapi.SetDetailCallbackStatus(false);
				
			}
			else{

				/* 
				Load in the heliostat field positions that are provided by the user.
				*/
				layout.heliostat_positions.clear();
				layout.heliostat_positions.resize(m_N_hel);
				
				for( int i=0; i<m_N_hel; i++)
				{
					layout.heliostat_positions.at(i).location.x = helio_positions(i,0);
					layout.heliostat_positions.at(i).location.y = helio_positions(i,1);
					if(pos_dim==3)
						layout.heliostat_positions.at(i).location.z = helio_positions(i,2);
					//layout.heliostat_positions.at(i).location.x = TCS_MATRIX_INDEX( var(P_helio_positions), i, 0 );
					//layout.heliostat_positions.at(i).location.y = TCS_MATRIX_INDEX( var(P_helio_positions), i, 1 );
					//if(pos_dim==3)
					//	layout.heliostat_positions.at(i).location.z = TCS_MATRIX_INDEX( var(P_helio_positions), i, 2);
					
				}

                sapi.Setup(amb, cost, layout, helios, recs);
								
			}
            //land area update
			ms_params.m_land_area = layout.land_area;		//value(P_land_area, layout.land_area);
            //number of heliostats
				//twn: get from matrix_t: ms_params.m_helio_positions
			//ms_params.m_N_hel = m_N_hel;					//value(P_N_hel, (double)m_N_hel);

			if(!mf_callback || !m_cdata)
				sapi.SetSummaryCallbackStatus(false);
			else
			{
				sapi.SetSummaryCallbackStatus(true);
				sapi.SetSummaryCallback(mf_callback, m_cdata);
			}

			

			//set up flux map resolution
			fluxtab.is_user_spacing = true;
			fluxtab.n_flux_days = n_flux_days;
			fluxtab.delta_flux_hrs = delta_flux_hrs;

			//run the flux maps
			if(! sapi.CalculateFluxMaps(fluxtab, m_n_flux_x, m_n_flux_y, true) )
			{
				throw(C_csp_exception("Simulation cancelled during fluxmap preparation","heliostat field initialization"));
                //message(TCS_ERROR, "Simulation cancelled during fluxmap preparation");
                //return -1;
            }

			//collect efficiencies
			sunpos.clear();
			effs.clear();
			int npos = (int)fluxtab.azimuths.size();
			sunpos.reserve(npos);
			effs.reserve(npos);

            //eta_map = allocate( P_eta_map, npos, 3, 0.);
			ms_params.m_eta_map.resize_fill(npos, 3.0, 0.0);

			m_flux_positions.resize(npos, VectDoub(2) );

			for(int i=0; i<npos; i++)
			{
				sunpos.push_back( vector<double>(2, 0.) );

				sunpos.back().at(0) = fluxtab.azimuths.at(i) / az_scale;
				sunpos.back().at(1) = fluxtab.zeniths.at(i) / zen_scale;
				effs.push_back( fluxtab.efficiency.at(i) / eff_scale );

                //fill the parameter matrix to return this data to calling program
                //also fill the flux sun positions matrix
                ms_params.m_eta_map(i,0) = m_flux_positions.at(i).at(0) = fluxtab.azimuths.at(i)*180./CSP::pi;
                ms_params.m_eta_map(i,1) = m_flux_positions.at(i).at(1) = fluxtab.zeniths.at(i)*180./CSP::pi;
                ms_params.m_eta_map(i,2) = fluxtab.efficiency.at(i);
			}

			//collect flux's
			//flux_maps = allocate( P_flux_maps, m_n_flux_y * npos, m_n_flux_x );
			ms_params.m_flux_maps.resize_fill(m_n_flux_y*npos, m_n_flux_x, 0.0);
			
			block_t<double> *f = &fluxtab.flux_surfaces.front().flux_data;

			int nfl = f->nlayers();

			for(int i=0; i<nfl; i++)
			{
				for(int j=0; j<m_n_flux_y; j++)
				{
					for(int k=0; k<m_n_flux_x; k++)
					{
						ms_params.m_flux_maps(i*m_n_flux_y + j, k) = f->at(j, k, i);
						//TCS_MATRIX_INDEX( var(P_flux_maps), i*m_n_flux_y + j, k) = f->at(j, k, i);
					}
				}
			}

			break;
		}
		case RUN_TYPE::USER_DATA:
		{

			//int nrows, ncols;
			//double *p_map = value( P_eta_map, &nrows, &ncols);
			int nrows = ms_params.m_eta_map.nrows();
			int ncols = ms_params.m_eta_map.ncols();
		
			if(ncols != 3)
			{
				error_msg = util::format("The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
					" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);

				throw(C_csp_exception(error_msg, "heliostat field initialization"));

				//message(TCS_ERROR,  "The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
				//	" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);
				//return -1;
			}
		
			//read the data from the array into the local storage arrays
			sunpos.resize(nrows, VectDoub(2));
			effs.resize(nrows);
			for(int i=0; i<nrows; i++)
			{
				sunpos.at(i).at(0) = eta_map(i, 0) / az_scale * CSP::pi / 180.0;
				sunpos.at(i).at(1) = eta_map(i, 1) / zen_scale * CSP::pi / 180.0;
				effs.at(i) = eta_map(i, 2) / eff_scale;
				//sunpos.at(i).at(0) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 0 ) / az_scale * CSP::pi/180.;
				//sunpos.at(i).at(1) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 1 ) / zen_scale * CSP::pi/180.;
				//effs.at(i) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 2 ) / eff_scale;
			}

            break;
		}
		default:
			break;
		}

		ms_outputs.m_flux_map_out.resize_fill(m_n_flux_y, m_n_flux_x, 0.0);

		//report back the flux positions used
		int nflux = (int)m_flux_positions.size();
		ms_params.m_flux_positions.resize_fill(nflux, 2, 0.0);
		//flux_positions = allocate(P_flux_positions, nflux, 2);
		
		for( int i = 0; i<nflux; i++ )
		{
			ms_params.m_flux_positions(i,0) = m_flux_positions.at(i).at(0);
			ms_params.m_flux_positions(i,1) = m_flux_positions.at(i).at(1);
			//flux_positions[i * 2] = m_flux_positions.at(i).at(0);
			//flux_positions[i * 2 + 1] = m_flux_positions.at(i).at(1);
		}

		/*
		------------------------------------------------------------------------------
		Create the regression fit on the efficiency map
		------------------------------------------------------------------------------
		*/

		//collect nug and beta
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;

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
			error_msg = util::format("The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);
			mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
			//message(TCS_WARNING, "The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);
		}
		// Initialize stored variables
		m_eta_prev = 0.0;
		m_v_wind_prev = 0.0;

		m_ncall = -1;
}

void C_pt_heliostatfield::call(const C_csp_weatherreader::S_outputs *p_weather, double field_control_in, const C_csp_solver_sim_info *p_sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;
	
	// Get sim info
	double time = p_sim_info->m_time;
	double step = p_sim_info->m_step;
	//int ncall = p_sim_info->m_ncall;

	double v_wind = p_weather->m_wspd;
	m_v_wind_current = v_wind;
	double field_control = field_control_in;	// Control Parameter ( range from 0 to 1; 0=off, 1=all on)
	if( field_control_in > 1.0 )
		field_control = 1.0;
	if( field_control_in < 0.0 )
		field_control = 0.0;

	double solzen = p_weather->m_solzen*CSP::pi / 180.0;

	if( solzen >= CSP::pi / 2.0 )
		field_control = 0.0;			// No tracking before sunrise or after sunset

	double solaz = p_weather->m_solazi*CSP::pi / 180.0;

	// clear out the existing flux map
	ms_outputs.m_flux_map_out.fill(0.0);

	// Parasitics for startup or shutdown
	double pparasi = 0.0;

	// If starting up or shutting down, calculate parasitics
	if( (field_control > 1.e-4 && m_eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && m_eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= m_v_wind_max) ||		// OR Shutdown by high wind speed
		(m_eta_prev > 1.e-4 && m_v_wind_prev >= m_v_wind_max && v_wind < m_v_wind_max) )	// OR Startup after high wind speed
		pparasi = m_N_hel * m_p_start / (step / 3600.0);			// kJ/hr 

	// Parasitics for tracking      
	if( v_wind < m_v_wind_max && m_v_wind_prev < m_v_wind_max )
		pparasi += m_N_hel * m_p_track * field_control;	// kJ/hr

	double eta_field = 0.;

	if( solzen > (CSP::pi / 2 - .001 - m_hel_stow_deploy) || v_wind > m_v_wind_max || time < 3601 )
	{
		eta_field = 1.e-6;
	}
	else
	{
		// Use current solar position to interpolate field efficiency table and find solar field efficiency
		vector<double> sunpos;
		sunpos.push_back(solaz / az_scale);
		sunpos.push_back(solzen / zen_scale);

		eta_field = field_efficiency_table->interp(sunpos) * eff_scale;
		eta_field = fmin(fmax(eta_field, 0.0), 1.0) * field_control;		// Ensure physical behavior 

		//Set the active flux map
		VectDoub pos_now(sunpos);
		/*VectDoub pos_now(2);
		pos_now.at(0) = solaz/az_scale;
		pos_now.at(1) = solzen/zen_scale;*/
		//find the nearest neighbors to the current point
		vector<double> distances;
		vector<int> indices;
		for( int i = 0; i<(int)m_flux_positions.size(); i++ ){
			distances.push_back(rdist(&pos_now, &m_flux_positions.at(i)));
			indices.push_back(i);
		}
		quicksort<double, int>(distances, indices);
		//calculate weights for the nearest 6 points
		double avepoints = 0.;
		const int npt = 6;
		for( int i = 0; i<npt; i++ )
			avepoints += distances.at(i);
		avepoints *= 1. / (double)npt;
		VectDoub weights(npt);
		double normalizer = 0.;
		for( int i = 0; i<npt; i++ ){
			double w = exp(-pow(distances.at(i) / avepoints, 2));
			weights.at(i) = w;
			normalizer += w;
		}
		for( int i = 0; i<npt; i++ )
			weights.at(i) *= 1. / normalizer;

		//set the values
		for( int k = 0; k<npt; k++ )
		{
			int imap = indices.at(k);
			for( int j = 0; j<m_n_flux_y; j++ )
			{
				for( int i = 0; i<m_n_flux_x; i++ )
				{
					ms_outputs.m_flux_map_out(j, i) += ms_params.m_flux_maps(imap*m_n_flux_y + j, i)*weights.at(k);
					//TCS_MATRIX_INDEX(var(O_flux_map), j, i) +=
					//	TCS_MATRIX_INDEX(var(P_flux_maps), imap*n_flux_y + j, i) * weights.at(k);
				}
			}
		}

	}

	ms_outputs.m_pparasi = pparasi / 3.E6;		//[MW], convert from kJ/hr: Parasitic power for tracking
	ms_outputs.m_eta_field = eta_field;			//[-], field efficiency

}

void C_pt_heliostatfield::converged()
{
	m_eta_prev = ms_outputs.m_eta_field;
	m_v_wind_prev = m_v_wind_prev;

	m_ncall = -1;
}

double C_pt_heliostatfield::rdist(VectDoub *p1, VectDoub *p2, int dim )
{
	double d = 0;
	for( int i = 0; i<dim; i++ ){
		double rd = p1->at(i) - p2->at(i);
		d += rd * rd;
	}
	return sqrt(d);
}