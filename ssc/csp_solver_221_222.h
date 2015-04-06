#ifndef __csp_solver_221_222_
#define __csp_solver_221_222_

#include "csp_solver_core.h"

#include "lib_util.h"
#include "lib_weatherfile.h"
#include <algorithm>

#include "interpolation_routines.h"
#include "AutoPilot_API.h"
#include "IOUtil.h"
#include "sort_method.h"

class C_csp_mspt_221_222 : public C_csp_collector_receiver
{
private:
	// private member data from 'sam_mw_pt_heliostat'

	// Class Instances
	GaussMarkov *field_efficiency_table;
	// Flux table
	sp_flux_table fluxtab;

	//Parameters
	string weather_file;
	int run_type;
	double helio_width;
	double helio_height;
	double helio_optical_error;
	double helio_active_fraction;
	double dens_mirror;
	double helio_reflectance;
	double rec_absorptance;
	double rec_height;
	double rec_aspect;
	double rec_hl_perm2;
	double q_design;
	double h_tower;
	int land_bound_type;
	double land_max;
	double land_min;
	double* land_bound_table;
	double* land_bound_list;
	double p_start;
	double p_track;
	double hel_stow_deploy;
	double v_wind_max;
	double interp_nug;
	double interp_beta;
	double* helio_positions;
	double* helio_aim_points;
	int N_hel, pos_dim;
	double* eta_map;
	int n_flux_x;
	int n_flux_y;
	double* flux_positions;
	MatDoub m_flux_positions;
	double* flux_maps;
	double* flux_map;
	double c_atm_0, c_atm_1, c_atm_2, c_atm_3;
	int n_facet_x, n_facet_y;
	int cant_type, focus_type;
	int n_flux_days, delta_flux_hrs;
	double dni_des;

	//Stored Variables
	double eta_prev;
	double v_wind_prev;

	virtual void exec() throw(general_error)
	{

	}

public:
	struct RUN_TYPE {
		enum A {
			AUTO, USER_FIELD, USER_DATA
		};
	};
	
	C_csp_mspt_221_222();

	virtual ~C_csp_mspt_221_222();

	virtual void init();
	
};








#endif	// __csp_solver_221_222_