#include "API_structures.h"
//#include <limits>

// ----------------- optimize --------------------
void sp_optimize::LoadDefaults(var_set &V)
{
	/* 
	Setup _variables to match the optimization settings
	*/
	

	//collect info on land scaling methods
	bool is_bounds_scaled = V["land"][0]["is_bounds_scaled"].value_bool();
	bool is_bounds_fixed = V["land"][0]["is_bounds_fixed"].value_bool();
	bool is_bounds_array = V["land"][0]["is_bounds_array"].value_bool();
	//check to make sure multiple options aren't selected. This isn't supported during optimization
	if( (is_bounds_array ? 1 : 0) + (is_bounds_fixed ? 1 : 0) + (is_bounds_scaled ? 1 : 0) > 1)
		throw spexception("Please choose a single land bounding method for optimization");
	//make sure we're not trying to optimize land bounds for a polygonal shape
	if( is_bounds_array )
		is_optimize_bound = false;
	else
		is_optimize_bound = true;

	//collect other range settings
	method = METHOD::DIRECT_L;
	double hmax,hmin;
	range_tht[0] = V["solarfield"][0]["tht_opt_min"].value_double();	//[m] {min, max}
	range_tht[1] = V["solarfield"][0]["tht_opt_max"].value_double();
	range_rec_height[0] = hmin = V["receiver"][0]["height_opt_min"].value_double();	//[m] {min, max}
	range_rec_height[1] = hmax = V["receiver"][0]["height_opt_max"].value_double();
	range_rec_aspect[0] = hmin/V["receiver"][0]["width_opt_max"].value_double();	//[m] {min, max}
	range_rec_aspect[1] = hmax/V["receiver"][0]["width_opt_min"].value_double();	//min and max aspect, respectively, use min height/max width, and max height/min width
		
	flux_max = V["receiver"][0]["peak_flux"].value_double();	//[kw/m2] Maximum allowable flux
	
	is_optimize_tht = true;
	is_optimize_rec_aspect = true;
	is_optimize_rec_height = true;
	
	is_range_constr_tht = false;
	is_range_constr_aspect = false;
	is_range_constr_rech = false;
	is_range_constr_bound = false;

	max_step_size = 0.05;
	converge_tol = 0.005;
}

// ----------------- ambient --------------------
void sp_ambient::LoadDefaults(var_set &V)
{
	site_elevation = V["ambient"][0]["elevation"].value_double();	//[m]
	site_latitude = V["ambient"][0]["latitude"].value_double();	//(+) North, (-) South
	site_longitude = V["ambient"][0]["longitude"].value_double();	//(-) West, (+) East
	site_time_zone = V["ambient"][0]["time_zone"].value_double();	//(-) West, (+) East

	AddWeatherStep(22,3,12.,950.,25.,3.,1.,1.);
	
	sun_type = V["ambient"][0]["sun_type"].value_int();
	
	atten_model = V["ambient"][0]["atm_model"].value_int();
}

string sp_ambient::weather_step::get_formatted_entry(){
	char line[200];
	sprintf(line, "[P]%d,%f,%d,%.2f,%.1f,%.1f,%.1f,%.2f", day_of_month, time_hours,
		month_of_year, dni, tdb, pres, vwind, step_weight);
	string sline = line;
	return sline;
}

// ----------------- heliostat --------------------



// ----------------- heliostat --------------------
void sp_heliostat::LoadDefaults(var_set &V)
{
	focus_type = V["heliostat"][0]["focus_method"].value_int();
	cant_type = V["heliostat"][0]["cant_method"].value_int();	
	user_focal_length = false;	//[m] (focus_type == USER_DEFINED)
	width = V["heliostat"][0]["width"].value_double();
	height = V["heliostat"][0]["height"].value_double();
	npanels_w = V["heliostat"][0]["n_cant_x"].value_int();
	npanels_h = V["heliostat"][0]["n_cant_y"].value_int();
	double 
		err_elevation = V["heliostat"][0]["err_elevation"].value_double(),
		err_azimuth = V["heliostat"][0]["err_azimuth"].value_double(),
		err_surface_x = V["heliostat"][0]["err_surface_x"].value_double(),
		err_surface_y = V["heliostat"][0]["err_surface_y"].value_double(),
		err_reflect_x = V["heliostat"][0]["err_reflect_x"].value_double(),
		err_reflect_y = V["heliostat"][0]["err_reflect_y"].value_double();
	
	optical_error = sqrt(
		4. * (
			err_elevation*err_elevation + 
			err_azimuth*err_azimuth +
			err_surface_x*err_surface_x +
			err_surface_y*err_surface_y ) +
		err_reflect_x*err_reflect_x +
		err_reflect_y*err_reflect_y ) / (2. *sqrt(2) ) ;  /* [rad] single-axis error ** SLOPE ** */

	active_fraction = V["heliostat"][0]["reflect_ratio"].value_double(); /* The fraction of the area width*height that reflects light */
	reflectance = 
		V["heliostat"][0]["reflectivity"].value_double() *
		V["heliostat"][0]["soiling"].value_double();
}

// ----------------- receiver --------------------
void sp_receiver::LoadDefaults(var_set &V)
{
	type = V["receiver"][0]["rec_type"].value_int();
	offset.Set(
		V["receiver"][0]["rec_offset_x"].value_double(),
		V["receiver"][0]["rec_offset_y"].value_double(),
		V["receiver"][0]["rec_offset_z"].value_double() );
	height = V["receiver"][0]["rec_height"].value_double();
	aspect = height/V["receiver"][0]["rec_width"].value_double();
	absorptance = V["receiver"][0]["absorptance"].value_double();
	q_hl_perm2 = V["receiver"][0]["therm_loss_base"].value_double();	//Receiver Heat loss [kWt] per square meter aperture area
	azimuth = V["receiver"][0]["rec_azimuth"].value_double();	//[optional]
	elevation = V["receiver"][0]["rec_elevation"].value_double();	//[optional]
}

// ----------------- layout --------------------

void sp_layout::LoadDefaults(var_set &V)
{
	q_design = V["solarfield"][0]["q_des"].value_double();	//Design power [MWt]
	land_bound_type = LAND_BOUND_TYPE::SCALED; //See enum LAND_BOUND_TYPE
	land_max = V["land"][0]["max_scaled_rad"].value_double();	//Land maximum radial extent [tht || m]
	land_min = V["land"][0]["min_scaled_rad"].value_double();	//Land minimum radial extent [tht || m]
	h_tower = V["solarfield"][0]["tht"].value_double();	//tower height [m]
	span_cw = V["solarfield"][0]["accept_max"].value_double(); //[optional] default=+180, field span in clockwise direction 
	span_ccw = V["solarfield"][0]["accept_min"].value_double();	//[optional] default=-180, field span in counterclockwise direction
}

void sp_layout::land_table::add_point(double x, double y, polygon &poly){
	coord p;
	p.x = x;
	p.y = y;
	poly.push_back(p);
}

void sp_layout::land_table::add_point(Point &P, polygon &poly){
	coord pp;
	pp.x = P.x;
	pp.y = P.y;
	poly.push_back(pp);
}

// ----------------- cost --------------------
void sp_cost::LoadDefaults(var_set &V)
{
	tower_fixed_cost = V["financial"][0]["tower_fixed_cost"].value_double();
	tower_exp = V["financial"][0]["tower_exp"].value_double();
	rec_ref_cost = V["financial"][0]["rec_ref_cost"].value_double();
	rec_ref_area = V["financial"][0]["rec_ref_area"].value_double();
	rec_cost_exp = V["financial"][0]["rec_cost_exp"].value_double();
	site_spec_cost = V["financial"][0]["site_spec_cost"].value_double();
	heliostat_spec_cost = V["financial"][0]["heliostat_spec_cost"].value_double();
	wiring_user_spec = V["financial"][0]["wiring_user_spec"].value_double();
	plant_spec_cost = V["financial"][0]["plant_spec_cost"].value_double();
	tes_spec_cost = V["financial"][0]["tes_spec_cost"].value_double();
	land_spec_cost = V["financial"][0]["land_spec_cost"].value_double();
	contingency_rate = V["financial"][0]["contingency_rate"].value_double();
	sales_tax_rate = V["financial"][0]["sales_tax_rate"].value_double();
	sales_tax_frac = V["financial"][0]["sales_tax_frac"].value_double();
	sales_tax_cost = V["financial"][0]["sales_tax_cost"].value_double();
}


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
