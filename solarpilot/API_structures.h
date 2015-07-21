#ifndef _API_STRUCTURES_
#define _API_STRUCTURES_ 1
#include "mod_base.h"
#include <vector>
#include <numeric>
#include <limits>
using namespace std;


struct sp_optimize
{
private:
	vector<vector<double> > _optimization_sim_points;
	vector<double>
		_optimization_objectives,
		_optimization_fluxes;

public:
	void LoadDefaults(var_set &V);

	struct METHOD { enum { BOBYQA=0, COBYLA=1, NEWOUA=2, NelderMead=3, Subplex=4, RSGS=5 }; };
	int method;
	double range_tht[2];	//[m] {min, max}
	double range_rec_aspect[2];	//[m] {min, max}
	double range_rec_height[2];	//[m] {min, max}
	double range_land_bound[2]; //[tht or m] {min, max} Allowable maximum land extent. Must be consistent with sp_layout::LAND_BOUND_TYPE. Not compatible with polygonal spec.
	double flux_max;	//[kw/m2] Maximum allowable flux
	
	bool is_optimize_tht;	//Allow tower height optimization
	bool is_optimize_rec_aspect;		//Allow receiver aspect (H/W) optimization
	bool is_optimize_rec_height;	//Allow receiver height optimization
	bool is_optimize_bound;		//Allow land bound (max radius) optimization. Used to balance flux intensity.
	
	bool is_range_constr_tht;	//Constrain allowable range for tower height
	bool is_range_constr_aspect;	//Constrain allowable range for receiver aspect ratio
	bool is_range_constr_rech;	//Constrain allowable range for receiver height
	bool is_range_constr_bound;	//Constrain allowable range for max land bound

	double converge_tol;	// Relative change in the objective function below which convergence is achieved
	double flux_penalty;	// Relative weight in the objective function given to flux intensity over the allowable limit
	int max_desc_iter;		// Maximum number of steps along the direction of steepest descent before recalculating the response surface
	int max_gs_iter;		// Maximum number of golden section iterations to refine the position of a local minimum
	int max_iter;			// Maximum number of times the optimization can iterate
	double max_step;		// Maximum total relative step size during optimization
	double power_penalty;  // Relative weight in the objective function given to power to the receiver below the required minimum

	void getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values);
	void setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values);
};

struct sp_ambient
{
private:
	struct weather_step
	{
		int day_of_month;	//1..[29-31]
		int month_of_year;	//1..12
		double time_hours;	//0..24, fractional time. Midpoint of the step
		double dni;		//W/m2
		double tdb;		//C
		double vwind;	//m/s
		double pres;	//bar
		double step_weight;	//[-]	relative duration of this time step
		string get_formatted_entry();
	};
	struct user_sun_step
	{
		double angle;
		double intensity;
	};
	struct sun_type_settings
	{
		double circumsolar_ratio;
		double gaussian_stdev;
		double pillbox_width;
		sun_type_settings(){
			//initialize to nonsense to check later whether these values are actually set
			circumsolar_ratio = std::numeric_limits<double>::quiet_NaN();
			gaussian_stdev = std::numeric_limits<double>::quiet_NaN();
			pillbox_width = std::numeric_limits<double>::quiet_NaN();
		};
	};
	
public:
	void LoadDefaults(var_set &V);

	double site_elevation;	//[m]
	double site_latitude;	//(+) North, (-) South
	double site_longitude;	//(-) West, (+) East
	double site_time_zone;	//(-) West, (+) East

	vector<weather_step> weather_data;
	void AddWeatherStep( int Day_of_month, int month_of_year, double Time_hours, double DNI, 
		double Tdb, double Vwind, double Pres, double Step_weight)
	{
		weather_step x;
		x.day_of_month = Day_of_month;
		x.month_of_year = month_of_year;
		x.time_hours = Time_hours;
		x.dni = DNI;
		x.tdb = Tdb;
		x.vwind = Vwind;
		x.pres = Pres;
		x.step_weight = Step_weight;
		weather_data.push_back( x );
	};

	struct SUN_TYPE { enum A { PILLBOX=2, GAUSSIAN=4, LIMB_DARKENED=1, POINT=0, BUIE=5, USER=3 }; };
	int sun_type;
	sun_type_settings  sun_type_params;
	vector<user_sun_step> user_sun_data;
	void AddUserSunStep( double Angle, double Intens ){
		user_sun_step x;
		x.angle = Angle;
		x.intensity = Intens;
		user_sun_data.push_back( x );
	}
	struct ATTEN_MODEL { enum A { DELSOL_CLEAR_DAY, DELSOL_HAZY_DAY, USER_DEFINED }; };
	
	int atten_model;
	vector<double> user_atten_coefs;

};

struct sp_heliostat
{
private:
	struct Cant_settings
	{
		int point_day;	//1..365 (AT_DAY_HOUR)
		int point_hour;	//0..23  (AT_DAY_HOUR)
		Vect point_vector;	// (USER_VECTOR)
		bool scale_with_slant;	// (USER_VECTOR)
	};
public:
	void LoadDefaults(var_set &V);

	struct FOCUS_TYPE { enum A { FLAT, AT_SLANT, USER_DEFINED }; };
	//struct CANT_TYPE { enum A {FLAT=0, AT_SLANT=-1, AT_DAY_HOUR=3, USER_VECTOR=4 }; };
    struct CANT_TYPE { enum A {NONE, ON_AXIS, EQUINOX, SOLSTICE_SUMMER, SOLSTICE_WINTER }; };
    //None,On-axis,Equinox,Summer Solstice,Winter Solstice
	int focus_type;
	int cant_type;	
	Cant_settings cant_settings;
	double user_focal_length;	//[m] (focus_type == USER_DEFINED)
	double width, height;
	int npanels_w, npanels_h;
	double optical_error;  /* [rad] single-axis error ** SLOPE ** */
	double active_fraction; /* The fraction of the area width*height that reflects light */
	double reflectance;

};
typedef vector<sp_heliostat> sp_heliostats;


struct sp_receiver
{
	void LoadDefaults(var_set &V);

	struct TYPE { enum A { CYLINDRICAL, CAVITY, FLAT}; };
	int type;
	struct { double x, y, z; } offset;
	double aspect, height;
	double absorptance;
	double q_hl_perm2;	//Receiver Heat loss [kWt] per square meter aperture area
	double 
		azimuth,	//[optional]
		elevation;	//[optional]
	
};
typedef vector<sp_receiver> sp_receivers;


struct sp_layout
{

	struct h_position
	{
        struct { double x, y, z; } 
            location, 
            aimpoint;
		int template_number; //0 based
		bool user_optics;	//Indicate whether the user will provide a cant/focus vector
        struct {double i, j, k; } cant_vector;	//[optional] Canting aim vector of total magnitude equal to the cant radius
		double focal_length;	//[optional] Heliostat focal length
	};

	void LoadDefaults(var_set &V);

	double q_design;	//Design power [MWt]
	double dni_design;	//Reference-point DNI [W/m2]
	unsigned int land_bound_type; //See enum LAND_BOUND_TYPE
	double 
		land_max,	//Land maximum radial extent [tht || m]
		land_min;	//Land minimum radial extent [tht || m]
	double land_area;		//Land area calculated by SolarPILOT
	double h_tower;	//tower height [m]
	double 
		span_cw, //[optional] default=+180, field span in clockwise direction 
		span_ccw;	//[optional] default=-180, field span in counterclockwise direction
    double area_sf;     //Total solar field reflective area
	
	struct LAND_BOUND_TYPE { enum A {SCALED, FIXED, POLYGON }; };
	
	vector<h_position> heliostat_positions;
	
	struct land_table
	{
	private:
		struct coord { double x,y; };
		typedef vector<coord> polygon;
		
	public:
		vector<polygon> inclusions, exclusions;
		void add_point(double x, double y, polygon &poly);
		void add_point(Point &P, polygon &poly);
	} landtable;	//[optional] object specifying land bounds
};

struct sp_cost
{
	void LoadDefaults(var_set &V);

	double tower_fixed_cost;
	double tower_exp;
	double rec_ref_cost;
	double rec_ref_area;
	double rec_cost_exp;
	double site_spec_cost;
	double heliostat_spec_cost;
	double wiring_user_spec;
	double plant_spec_cost;
	double tes_spec_cost;
	double land_area_const;	//acres
	double land_area_mult;	
	double land_spec_cost;
	double contingency_rate;
	double sales_tax_rate;
	double sales_tax_frac;
	double sales_tax_cost;
    double cost_fixed;

    //calculated values
    double cost_rec_tot;
    double cost_tower_tot;
    double cost_land_tot;
    double cost_heliostat_tot;
    double cost_site_tot;
    double cost_plant_tot;
    double cost_tes_tot;
    double cost_fossil_tot;
    double cost_salestax_tot;
    double cost_direct_tot;
    double cost_epc_tot;
    double cost_indirect_tot;
    double cost_installed_tot;
};


struct sp_optical_table 
{
	/* 
	Optical table stores whole-field optical efficiency as a function of 
	solar azimuth and zenith angles.
	*/
	sp_optical_table();
	bool is_user_positions;		//user will specify azimuths and zeniths
	vector<double> zeniths;
	vector<double> azimuths;
	vector<vector<double> > eff_data;
};

struct sp_flux_map
{
	struct sp_flux_stack
	{
		string map_name;
		vector<double> xpos;
		vector<double> ypos;
		block_t<double> flux_data;
	};
	vector<sp_flux_stack> flux_surfaces;  

};

struct sp_flux_table : sp_flux_map
{
	/* 
	Flux table stores flux maps for each receiver & receiver surface (if applicable) 
	for the annual set of sun azimuth and zenith angles. 
	*/
	
	sp_flux_table();

	bool is_user_spacing;	//user will specify data in 'n_flux_days' and 'delta_flux_hours'
	int n_flux_days;		//How many days are used to calculate flux maps? (default = 8)
	double delta_flux_hrs;		//How much time (hrs) between each flux map? (default = 1)
	//-- data calculated by the algorithm:
	
	vector<double> azimuths;
	vector<double> zeniths;
	vector<double> efficiency;
	//---
};


#endif
