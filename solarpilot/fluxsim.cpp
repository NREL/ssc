#include "fluxsim.h"

void FluxSimData::Create(var_map &V)
{
	setVar("aim_method", _aim_method, V, 3);		//Method for determining the aim point for each heliostat
	setVar("class_name", _class_name, V, "Performance Simulation");		//Class name
	setVar("cloud_depth", _cloud_depth, V, 250.);		//Depth of the cloud shape
	setVar("cloud_loc_x", _cloud_loc_x, V, 200.);		//Base location of the cloud(s) relative to the tower position - X dimension
	setVar("cloud_loc_y", _cloud_loc_y, V, 200.);		//Base location of the cloud(s) relative to the tower position - Y dimension
	setVar("cloud_opacity", _cloud_opacity, V, 0.8, "[0,1]");		//Fraction of DNI obfuscated by a cloud shadow
	setVar("cloud_sep_depth", _cloud_sep_depth, V, 1.);		//Cloud pattern depth spacing
	setVar("cloud_sep_width", _cloud_sep_width, V, 2.);		//Cloud pattern width spacing
	setVar("cloud_shape", _cloud_shape, V, 0);		//Shape used to model the cloud shadow
	setVar("cloud_skew", _cloud_skew, V, 35., "[-180,180]");		//Angle between North and the depth direction (-180 to +180 with clockwise positive)
	setVar("cloud_width", _cloud_width, V, 500.);		//Width of the cloud shape
	setVar("flux_day", _flux_day, V, 20, "[1,31]");		//Day of the month for the flux simulation
	setVar("flux_dist", _flux_dist, V, 0);		//Sampling basis for random positioning. Non-uniform distributions are weighted away from the center.
	setVar("flux_hour", _flux_hour, V, 12., "[0,24)");		//Hour of the day for the flux simulation
	setVar("flux_model", _flux_model, V, 0);		//Desired flux simulation tool. Not all geometries can be simulated using the Hermite approximation.
	setVar("flux_month", _flux_month, V, 3, "[1,12]");		//Month of the year for the flux simulation
	setVar("flux_solar_az", _flux_solar_az, V, 180.);		//Solar azimuth angle to use for the flux simulation
	setVar("flux_solar_az_in", _flux_solar_az_in, V, 180., "[-180,180]");		//Solar azimuth angle to use for the flux simulation
	setVar("flux_solar_el", _flux_solar_el, V, 85.);		//Solar elevation angle to use for the flux simulation
	setVar("flux_solar_el_in", _flux_solar_el_in, V, 85., "[0,90]");		//Solar elevation angle to use for the flux simulation
	setVar("flux_time_type", _flux_time_type, V, 1);		//Method for specifying the desired flux simulation time.
	setVar("flux_dni", _flux_dni, V, 950.);		//Direct Normal Irradiation at the specified simulation point
	setVar("is_cloud_pattern", _is_cloud_pattern, V, true);		//Create a pattern based on the specified cloud
	setVar("is_cloud_symd", _is_cloud_symd, V, true);		//Mirror the cloud pattern below the width axis
	setVar("is_cloud_symw", _is_cloud_symw, V, true);		//Mirror the cloud pattern to the left of the depth axis
	setVar("is_cloudy", _is_cloudy, V, false);		//Enable simulation for a cloud transient
	setVar("is_optical_err", _is_optical_err, V, true);		//Include the reflector optical error sources in the SolTrace simulation
	setVar("is_sunshape_err", _is_sunshape_err, V, true);		//Include the sun shape error in the SolTrace simulation
	setVar("max_rays", _max_rays, V, 1000000, "[1,1e99]");		//The maximum number of generated rays allowed before terminating the simulation. Overrides the desired rays setting.
	setVar("min_rays", _min_rays, V, 10000, "[1,1e99]");		//The minimum number of ray hits on the receiver before terminating the simulation.
	setVar("norm_dist_sigma", _norm_dist_sigma, V, 0.25, "(0.,1.]");		//Size of the standard distribution relative to half of the height of the receiver.
	setVar("save_data", _save_data, V, false);		//Save the results for each ray
	setVar("save_data_loc", _save_data_loc, V, "soltrace_ray_data.csv");		//Choose a location to save the ray data
	setVar("seed", _seed, V, -1, "[-1, 9e9]");		//The seed for the random number generator
	setVar("sigma_limit", _sigma_limit, V, 2., "[0.,1000]");		//Image positioning cutoff - desired distance of each image in standard deviations from the receiver edge
	setVar("x_res", _x_res, V, 25, "[1,1000]");		//Number of flux test points per panel (maximum) in the vertical direction for the flux simulation
	setVar("y_res", _y_res, V, 25, "[1,1000]");		//Number of flux test points per panel (maximum) in the horizontal direction for the flux simulation
	setVar("is_autoscale", _is_autoscale, V, true);		//Autoscale the Z-axis of the contour plot
	setVar("plot_zmax", _plot_zmax, V, 1000.);		//Z-axis maximum value
	setVar("plot_zmin", _plot_zmin, V, 0.);		//Z-axis minimum value
	setVar("flux_data", _flux_data, V, "");		//2D matrix of flux data
}