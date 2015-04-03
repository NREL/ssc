#include "tcstype.h"
#include "csp_solver_core.h"

#include "lib_util.h"

#include "lib_weatherfile.h"
#include <algorithm>

#include "interpolation_routines.h"

#include "htf_props.h"

using namespace std;

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

		P_ffrac,
		P_field_fl_props,

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

	{ TCS_PARAM,    TCS_ARRAY,    P_ffrac,              "ffrac",                "Fossil dispatch logic",                                   "-",            "",        "",        ""},
	{ TCS_PARAM,    TCS_MATRIX,   P_field_fl_props,     "field_fl_props",       "User defined field fluid property data",                  "-",            "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	}
};

C_csp_mspt_221_222::C_csp_mspt_221_222()
{
	set_params_and_size_vector(csp_solver_221_222_params);
}

void C_csp_mspt_221_222::init()
{
	double helio_width = value(P_helio_width);
	
	double *ffrac;

	int l_ffrac = 0;
	ffrac = value(P_ffrac, l_ffrac);

	double abc = ffrac[8];

	double should_be_weird = ffrac[9];

	HTFProperties field_htfProps;

	int nrows = 0, ncols = 0;
	double *fl_mat = value(P_field_fl_props, nrows, ncols);
	if( fl_mat != 0 && nrows > 2 && ncols == 7 )
	{
		util::matrix_t<double> mat(nrows, ncols, 0.0);
		for( int r = 0; r<nrows; r++ )
			for( int c = 0; c<ncols; c++ )
				mat(r,c) = fl_mat[ncols*r + c];
	
		if( !field_htfProps.SetUserDefinedFluid(mat) )
		{
			double blah = 12.34;
		//	//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
		//	message(TCS_ERROR, field_htfProps.UserFluidErrMessage(), nrows, ncols);
		//	return -1;
		}
	}
	else
	{
		double uh_huh = 1.23;
		//message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
		//return -1;
	}


	int agda = 1;
}
