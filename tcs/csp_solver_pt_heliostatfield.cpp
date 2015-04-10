#include "csp_solver_pt_heliostatfield.h"
#include "sam_csp_util.h"

C_pt_heliostatfield::C_pt_heliostatfield()
{
	m_p_start = m_p_track = m_hel_stow_deploy = m_v_wind_max = std::numeric_limits<double>::quiet_NaN();

	m_n_flux_x = m_n_flux_y = m_N_hel = -1;
}

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
	double *land_bound_table = NULL;
	double *land_bound_list = NULL;
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

	double *helio_positions = NULL;
	double *helio_aim_points = NULL;
	double *eta_map = NULL;
	double *flux_positions = NULL;
	double *flux_maps = NULL;

	int pos_dim = 0;

	double *flux_map = NULL;

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
		nrows1 = ms_params.m_nrows_land_bound_table;
		ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);

		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = ms_params.m_nrows_land_bound_list;
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
		m_N_hel = ms_params.m_N_hel;
		pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = ms_params.m_nrows_helio_aim_points;
		ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);

		break;
	case RUN_TYPE::USER_DATA:

		h_tower = ms_params.m_h_tower;
		land_bound_type = ms_params.m_land_bound_type;
		land_max = ms_params.m_land_max;
		land_min = ms_params.m_land_min;
		
		land_bound_table = ms_params.m_land_bound_table;
		nrows1 = ms_params.m_nrows_land_bound_table;
		ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);
		
		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = ms_params.m_nrows_land_bound_list;
		//land_bound_list = value(P_land_bound_list, &nrows2);
		
		m_p_start = ms_params.m_p_start;
		m_p_track = ms_params.m_p_track;
		m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;
		m_v_wind_max = ms_params.m_v_wind_max;
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;

		helio_positions = ms_params.m_helio_positions;
		m_N_hel = ms_params.m_N_hel;
		pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = ms_params.m_nrows_helio_aim_points;
		ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
		
		eta_map = ms_params.m_eta_map;
		nrows5 = ms_params.m_nrows_eta_map;
		ncols5 = ms_params.m_ncols_eta_map;
		//eta_map = value(P_eta_map, &nrows5, &ncols5);

		m_n_flux_x = ms_params.m_n_flux_x;
		m_n_flux_y = ms_params.m_n_flux_y;
		
		flux_positions = ms_params.m_flux_positions;
		nfluxpos = ms_params.m_nfluxpos;
		nfposdim = ms_params.m_nfposdim;
		//flux_positions = value(P_flux_positions, &nfluxpos, &nfposdim);

		flux_maps = ms_params.m_flux_maps;
		nfluxmap = ms_params.m_nfluxmap;
		nfluxcol = ms_params.m_nfluxcol;
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
}