#include <stdio.h>

#include "API_structures.h"
#include "exceptions.hpp"
#include "definitions.h"

using namespace std;

// ----------------- optimize --------------------
/*
void sp_optimize::LoadDefaults(var_map &V)
{
	//Setup _variables to match the optimization settings
	

	//collect info on land scaling methods
	bool is_bounds_scaled = V.land.is_bounds_scaled.val;
	bool is_bounds_fixed = V.land.is_bounds_fixed.val;
	bool is_bounds_array = V.land.is_bounds_array.val;
	//check to make sure multiple options aren't selected. This isn't supported during optimization
	if( (is_bounds_array ? 1 : 0) + (is_bounds_fixed ? 1 : 0) + (is_bounds_scaled ? 1 : 0) > 1)
		throw spexception("Please choose a single land bounding method for optimization");
	//make sure we're not trying to optimize land bounds for a polygonal shape
	if( is_bounds_array )
		is_optimize_bound = false;
	else
		is_optimize_bound = V.land.is_land_max_opt.val;
    double max_rad;
    if( is_bounds_fixed ) 
    {
        max_rad = V.land.max_fixed_rad.val;
    }
    else if( is_bounds_scaled )
    {
        max_rad = V.land.max_scaled_rad.val;
    }
    range_land_bound[0] = V.land.land_max_opt_min.val * max_rad;
    range_land_bound[1] = V.land.land_max_opt_max.val * max_rad;

	//collect other range settings
	method = V.opt.algorithm.val;
    double tht = V.sf.tht.val;
	range_tht[0] = V.sf.tht_opt_min.val * tht;	//[m] {min, max}
	range_tht[1] = V.sf.tht_opt_max.val * tht;
    double rec_height = V.recs[0].rec_height.val;
	range_rec_height[0] = V.recs[0].height_opt_min.val * rec_height;	//[m] {min, max}
	range_rec_height[1] = V.recs[0].height_opt_max.val * rec_height;
	double rec_aspect = V.recs[0].rec_aspect.Val();
    range_rec_aspect[0] = V.recs[0].aspect_opt_min.val * rec_aspect;	//[m] {min, max}
	range_rec_aspect[1] = V.recs[0].aspect_opt_max.val * rec_aspect;	
	

	flux_max = V.recs[0].peak_flux.val;	//[kw/m2] Maximum allowable flux
	
	is_optimize_tht = V.sf.is_tht_opt.val;
	is_optimize_rec_aspect = V.recs[0].is_aspect_opt.val;
	is_optimize_rec_height = V.recs[0].is_height_opt.val;
	

	is_range_constr_tht = V.sf.is_tht_restrict.val;
	is_range_constr_aspect = V.recs[0].is_aspect_restrict.val;
	is_range_constr_rech = V.recs[0].is_height_restrict.val;
	is_range_constr_bound = V.land.is_land_max_restrict.val;

	converge_tol = V.opt.converge_tol.val;
	flux_penalty = V.opt.flux_penalty.val;
	max_desc_iter = V.opt.max_desc_iter.val;
	max_gs_iter = V.opt.max_gs_iter.val;
	max_iter = V.opt.max_iter.val;
	max_step = V.opt.max_step.val;
	power_penalty = V.opt.power_penalty.val;

}
*/

void sp_optimize::getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	/* 
	Return the addresses of the optimization simulation history data, if applicable.
	*/
	sim_points = _optimization_sim_points;
	obj_values = _optimization_objectives;
	flux_values = _optimization_fluxes;
}

void sp_optimize::setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	//Create local copies
	_optimization_sim_points = sim_points;
	_optimization_objectives = obj_values;
	_optimization_fluxes = flux_values;
}

// ----------------- ambient --------------------
//void sp_ambient::LoadDefaults(var_map &V)
//{
//	site_elevation = V.amb.elevation.val;	//[m]
//	site_latitude = V.amb.latitude.val;	//(+) North, (-) South
//	site_longitude = V.amb.longitude.val;	//(-) West, (+) East
//	site_time_zone = V.amb.time_zone.val;	//(-) West, (+) East
//
//	AddWeatherStep(22,3,12.,950.,25.,3.,1.,1.);
//	
//	sun_type = V.amb.sun_type.val;
//	
//	atten_model = V.amb.atm_model.val;
//}
//
//string sp_ambient::weather_step::get_formatted_entry(){
//	char line[200];
//	sprintf(line, "[P]%d,%f,%d,%.2f,%.1f,%.1f,%.1f,%.2f", day_of_month, time_hours,
//		month_of_year, dni, tdb, pres, vwind, step_weight);
//	string sline = line;
//	return sline;
//}

// ----------------- heliostat --------------------
//void sp_heliostat::LoadDefaults(var_map &V)
//{
//	focus_type = V.hels[0].focus_method.val;
//	cant_type = V.hels[0].cant_method.val;	
//	user_focal_length = false;	//[m] (focus_type == USER_DEFINED)
//	width = V.hels[0].width.val;
//	height = V.hels[0].height.val;
//	npanels_w = V.hels[0].n_cant_x.val;
//	npanels_h = V.hels[0].n_cant_y.val;
//	double 
//		err_elevation = V.hels[0].err_elevation.val,
//		err_azimuth = V.hels[0].err_azimuth.val,
//		err_surface_x = V.hels[0].err_surface_x.val,
//		err_surface_y = V.hels[0].err_surface_y.val,
//		err_reflect_x = V.hels[0].err_reflect_x.val,
//		err_reflect_y = V.hels[0].err_reflect_y.val;
//	
//	optical_error = sqrt(
//		4. * (
//			err_elevation*err_elevation + 
//			err_azimuth*err_azimuth +
//			err_surface_x*err_surface_x +
//			err_surface_y*err_surface_y ) +
//		err_reflect_x*err_reflect_x +
//		err_reflect_y*err_reflect_y ) / (2. *sqrt(2) ) ;  /* [rad] single-axis error ** SLOPE ** */
//
//	active_fraction = V.hels[0].reflect_ratio.val; /* The fraction of the area width*height that reflects light */
//	reflectance = V.hels[0].reflectivity.val * V.hels[0].soiling.val;
//}

// ----------------- receiver --------------------
//void sp_receiver::LoadDefaults(var_map &V)
//{
//	type = V.recs[0].rec_type.val;
//	offset.x = V.recs[0].rec_offset_x.val;
//    offset.y = V.recs[0].rec_offset_y.val;
//	offset.z = V.recs[0].rec_offset_z.val;
//
//	height = V.recs[0].rec_height.val;
//    double width; 
//    
//    switch (V.recs[0].rec_type.val) 
//    {
//    case sp_receiver::TYPE::CYLINDRICAL:
//        width = V.recs[0].rec_diameter.val;
//        break;
//    case sp_receiver::TYPE::CAVITY:
//    case sp_receiver::TYPE::FLAT:
//        width = V.recs[0].rec_width.val;
//        break;
//    }
//
//    aspect = height/width;
//	absorptance = V.recs[0].absorptance.val;
//	q_hl_perm2 = V.recs[0].therm_loss_base.val;	//Receiver Heat loss [kWt] per square meter aperture area
//	azimuth = V.recs[0].rec_azimuth.val;	//[optional]
//	elevation = V.recs[0].rec_elevation.val;	//[optional]
//}

// ----------------- layout --------------------

//void sp_layout::LoadDefaults(var_map &V)
//{
//	q_design = V.sf.q_des.val;	//Design power [MWt]
//	dni_design = V.sf.dni_des.val;	//Reference-point DNI [W/m2]
//	land_bound_type = LAND_BOUND_TYPE::SCALED; //See enum LAND_BOUND_TYPE
//	land_max = V.land.max_scaled_rad.val;	//Land maximum radial extent [tht || m]
//	land_min = V.land.min_scaled_rad.val;	//Land minimum radial extent [tht || m]
//	h_tower = V.sf.tht.val;	//tower height [m]
//	span_cw = V.sf.accept_max.val; //[optional] default=+180, field span in clockwise direction 
//	span_ccw = V.sf.accept_min.val;	//[optional] default=-180, field span in counterclockwise direction
//    area_sf = 0.;   //calculated value -- initialize
//}
//
//void sp_layout::land_table::add_point(double x, double y, polygon &poly){
//	coord p;
//	p.x = x;
//	p.y = y;
//	poly.push_back(p);
//}
//
//void sp_layout::land_table::add_point(Point &P, polygon &poly){
//	coord pp;
//	pp.x = P.x;
//	pp.y = P.y;
//	poly.push_back(pp);
//}

// ----------------- cost --------------------
//void sp_cost::LoadDefaults(var_map &V)
//{
//	tower_fixed_cost = V.fin.tower_fixed_cost.val;
//	tower_exp = V.fin.tower_exp.val;
//	rec_ref_cost = V.fin.rec_ref_cost.val;
//	rec_ref_area = V.fin.rec_ref_area.val;
//	rec_cost_exp = V.fin.rec_cost_exp.val;
//	site_spec_cost = V.fin.site_spec_cost.val;
//	heliostat_spec_cost = V.fin.heliostat_spec_cost.val;
//	wiring_user_spec = V.fin.wiring_user_spec.val;
//	plant_spec_cost = V.fin.plant_spec_cost.val;
//	tes_spec_cost = V.fin.tes_spec_cost.val;
//	land_spec_cost = V.fin.land_spec_cost.val;
//	contingency_rate = V.fin.contingency_rate.val;
//	sales_tax_rate = V.fin.sales_tax_rate.val;
//	sales_tax_frac = V.fin.sales_tax_frac.val;
//	sales_tax_cost = V.fin.sales_tax_cost.Val();
//    cost_fixed = V.fin.fixed_cost.val;
//
//    //Initialize calculated values
//    cost_rec_tot = 0.;
//    cost_tower_tot = 0.;
//    cost_land_tot = 0.;
//    cost_heliostat_tot = 0.;
//    cost_site_tot = 0.;
//    cost_plant_tot = 0.;
//    cost_tes_tot = 0.;
//    cost_fossil_tot = 0.;
//    cost_salestax_tot = 0.;
//    cost_direct_tot = 0.;
//    cost_epc_tot = 0.;
//    cost_indirect_tot = 0.;
//    cost_installed_tot = 0.;
//}


// ----------------- flux table --------------------
sp_flux_table::sp_flux_table()
{
	is_user_spacing = false;
}

// ----------------- optical table --------------------
sp_optical_table::sp_optical_table()
{
	is_user_positions = false;
}
