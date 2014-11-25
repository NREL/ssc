#ifndef _SOLARFIELD_H_
#define _SOLARFIELD_H_ 1

#include <vector>
#include <string>

#include "string_util.h"
#include "interop.h"

#include "mod_base.h"
//#include "sort_method.h"
#include "Heliostat.h"
#include "Receiver.h"
#include "Financial.h"
#include "Ambient.h"
#include "Land.h"
#include "Plant.h"
#include "Flux.h"
#include "OpticalMesh.h"


struct LAYOUT_DETAIL
{
	enum A {
	//Subset of days/hours=2;Single simulation point=1;Do not filter heliostats=0;Annual simulation=3;Limited annual simulation=4;Representative profiles=5;Map to Annual=6
	//Associated with _des_sim_detail
	NO_FILTER=0,
	SINGLE_POINT,
	SUBSET_HOURS,
	FULL_ANNUAL,
	LIMITED_ANNUAL,
	AVG_PROFILES,
	MAP_TO_ANNUAL,
	FOR_OPTIMIZATION
	};
};

using namespace std;

//Declare any referenced classes first
class Receiver;
class Heliostat;
class Flux;
class Plant;
class LayoutSimThread;

class Ambient;
class Land;
class Plant;

typedef vector<Heliostat*> Hvector;
class sim_result;
typedef vector<sim_result> sim_results;

struct layout_obj
{
	/*
	This is a lightweight object that contains the information required to build a full heliostat object. 
	These objects are stored in a layout_shell type to keep track of the various layouts that are built
	during the simulation, but without building up large numbers of memory-intensive heliostat objects.
	*/

	int helio_type;
	Point 
		location,
		aim;
	double
		focal_x,
		focal_y;
	Vect
		cant;
	bool
		is_user_cant,	//Data provided on canting
		is_user_aim,	//Data provided on aiming
		is_user_focus;	//Data provided on focusing
};

typedef vector<layout_obj> layout_shell;
typedef map<int, Heliostat*> htemp_map;

/*The SolarField class will serve as the object that binds together all of the aspects of the solar field
into a single object. Multiple instances of the SolarField will be possible, and this will allow the 
maintenance and reference to a number of unique layouts and field constructions simultaneously.*/
class SolarField : public mod_base
{
protected:
	double 
		_tht, //[m] Optical height of the receiver compared to the heliostat pivot point
		_towD, //[m] Shadow width from the tower/receiver onto the field
		_towL, //[m] Maximum shadow height from the tower/receiver. Scaled by solar incidence angle.
		_accept_max, // [rad] Maximum acceptance angle - 0 = North +CW
		_accept_min, // [rad] Minimum acceptance angle - 0 = North -CCW
		_q_des, //[MW] Design-point solar field thermal rating
		_dni_des,	//DNI value at which the design-point receiver thermal power is achieved
		_sun_loc_des, 		//Sun location when thermal power rating is achieved
		_sun_loc_des_az, 	//Solar azimuth angle at the design point
		_sun_loc_des_el, 	//Solar elevation angle at the design point
		_az_spacing,	//[-] Azimuthal spacing factor of the first row after reset, multiply by heliostat width to get actual spacing
		_spacing_reset,	//[-] Heliostat azimuthal spacing ratio.. Indicates how much spacing should expand before starting a new compact row
		_row_spacing_x,		//Separation between adjacent heliostats in the X-direction, multiplies heliostat radius
		_row_spacing_y,		//Separation between adjacent heliostats in the Y-direction, multiplies heliostat radius
		_xy_rect_aspect,		//Aspect ratio of the rectangular field layout (height in Y / width in X)
		_prox_filter_frac, 		//Fraction of heliostats to subject to proximity filter.
		_q_to_rec,		//[MW] Power to the receiver during performance runs
		_q_des_opt_min, 		//Minimum possible solar field design power during optimzation
		_q_des_opt_max,			//Maximum possible solar field design power during optimzation
		_tht_opt_min, 		//Minimum allowable tower height during optimization
		_tht_opt_max, 		//Maximum allowable tower height during optimization
		_sim_time_step,		//Simulation weather data time step
		_sim_p_to_rec,	//Simulated power to the receiver. Store for plotting and reference
		_max_zone_size_rad, 		//Maximum zone size (radial direction) for grouping optical intercept factor calculations
		_max_zone_size_az, 		//Maximum zone size (azimuthal direction) for grouping optical intercept factor calculations
		_min_zone_size_rad,		//Minimum zone size (radial direction) for grouping optical intercept factor calculations
		_min_zone_size_az, 		//Minimum zone size (azimuthal direction) for grouping optical intercept factor calculations
		_zone_div_tol,		//Allowable variation in optical intercept factor within a layout zone
		_estimated_annual_power,	//Calculated in ProcessLayoutResults().. Estimate of total annual heliostat power output
		_q_des_withloss;			//[MW] The design point thermal power that must be met

	bool
		_is_power_opt, 		//Vary the power output during optimization to identify optimal level?
		_is_tht_opt,		//Vary the tower height during optimization to identify optimal level?
		_is_power_restrict,		//Restrict the search range for power output to the indicated limits
		_is_tht_restrict,		//Restrict the search range for tower height to the indicated limits
		_is_aimpoints_updated,	//Are the heliostat field aim points up to date?
		_is_prox_filter,		//Post-process the layout to select heliostats that are closer to the tower.
		_cancel_flag,	//Flag indicating the current simulation should be cancelled
		_is_created,	//Has the solar field Create() method been called?
		_is_opt_zoning;		//Enables grouping of heliostats into zones for intercept factor calculation during layout only

	
	int 
		_des_sim_detail, //Which method to determine heliostat performance during layout? (SEE "LAYOUT" enum for definitions)
		_des_sim_ndays, //For limited annual simulation, the number of evenly spaced days to simulate
		_layout_method, //[-] Integer flag indicating the type of layout method used
						//1=Radial stagger, 2=cornfield rows, 3=user(?)
		_rad_spacing_method,	//[-] Method to use for radial spacing in radial-stagger layout
		_xy_field_shape,		//Enforced shape of the heliostat field
		_template_rule,		//Method for distributing heliostat geometry templates in the field
		_hsort_method,		//method for sorting the heliostats during layout
		_des_sim_nhours;	//Limit the number of simulation hours to this
	string
		_layout_data;	//Layout data in string form. Refer to the parseHeliostatXYZFile() method
	WeatherData
		_sim_step_data;		//Data used for design simulations
	

	double _helio_extents[4];	//Extents of the heliostat field [xmax, xmin, ymax, ymin]
	layout_shell _layout;	//All of the layouts associated with this solar field
	vector<Heliostat> _helio_objects; //A matrix of heliostats for each position that's evaluated in the field. 
										  //This matrix stores all of the actual heliostat objects. These are pointed 
										  //to by the _heliostats array
	htemp_map _helio_templates;	//A map from the heliostat template integer type to the actual object in memory
	vector<Heliostat> _helio_template_objects;	//Actual heliostat objects
	unordered_map<int,Heliostat*> _helio_by_id;	//map of heliostats by ID#
	Hvector _heliostats; //A vector containing all of the heliostats in the field that are used in calculation
	matrix_t<Hvector> 
		_helio_groups,	//A 2-D mesh containing vectors that list the heliostats in each field group.
		_neighbors;	//A 2-D mesh where each node lists the heliostats that neighbor each other
	vector<Hvector> _layout_groups; //a vector of heliostat vectors that share flux intercept factor during layout calculations
	vector<Receiver*> _receivers; //A vector containing all of the receiver objects
	vector<Receiver*> _active_receivers;	//A vector containing only active receivers
	Ambient _ambient;
	Land _land;
	Financial _financial;
	Plant _plant;
	Flux *_flux;	/*This object is a pointer because it has a recursive relationship to the SolarField object.
					  See the SolarField constructor for the associated _flux constructor. Also, the object must
					  be allocated and freed from memory manually.*/

	simulation_info _sim_info;
	simulation_error _sim_error;

	optical_hash_tree _optical_mesh;

	class clouds : public mod_base
	{ 
		//members
		bool _is_cloudy;
		bool _is_cloud_pattern;
		bool _is_cloud_symd;
		bool _is_cloud_symw;
		int _cloud_shape;
		double _cloud_width;
		double _cloud_depth;
		double _cloud_opacity;
		double _cloud_skew;
		double _cloud_sep_width;
		double _cloud_sep_depth;
		double _cloud_loc_x;
		double _cloud_loc_y;
		vector<Point> _all_locs;

	public:
		struct SHAPE { enum A {ELLIPTICAL, RECTANGULAR, FRONT}; }; //local enum
		//methods
		void Create(var_map &V, double extents[2]);
		double ShadowLoss(Point &hloc);
		bool isCloudy();
	} _clouds;

public:

    struct HELIO_SPACING_METHOD { enum A {DELSOL_EMPIRICAL=1, NO_BLOCKING=2}; };
    struct SUNPOS_DESIGN { enum A {SOLSTICE_S, EQUINOX, SOLSTICE_W, ZENITH, USER }; };

	//Constructors - destructor
	SolarField (); //constructor

	SolarField( const SolarField &sf );

	~SolarField ();
	
	//-------Access functions
	//"GETS"
	double getDesignPower();
	int getLayoutMethod();
	int getHelioSortMethod();
	vector<Receiver*> *getReceivers();
	Land *getLandObject();
	Ambient *getAmbientObject();
	Flux *getFluxObject();
	Financial *getFinancialObject();
	Plant *getPlantObject();
	htemp_map *getHeliostatTemplates();
	Hvector *getHeliostats();
	layout_shell *getLayoutShellObject();
	unordered_map<int,Heliostat*> *getHeliostatsByID();
	vector<Heliostat> *getHeliostatObjects();
	clouds *getCloudObject();
	
	double getTowerHeight(); //[m] Returns optical height at the midpoint of the receiver
	double getTowerShadowL(); //[m] Returns the tower shadow base length
	double getTowerShadowW(); //[m] Returns the tower shadow width
	void getTowerShadow(double ShadWL[2]) ; //Returns the Length=2 array with [Width, Length]
	double getHeliostatArea();	//[m2] returns the total aperture area of the heliostat field
	double *getPlotBounds(bool use_land=false);	//Returns a pointer to an [4] array [xmax, xmin, ymax, ymin]
	void getLayoutAcceptAngles(double angles[2]);
	double getDesignPointDNI();
	bool getAimpointStatus();
	double getSimulatedPowerToReceiver();
	double getReceiverTotalArea();
	int getTemplateRule();
	double *getHeliostatExtents();
	int getDesignSimDetail();
	WeatherData *getSimulationStepData();
	void copySimulationStepData(WeatherData &wdata);
	void getSunPosDesignUser(double pos[2]);	//fills pos={azimuth, elevation} [deg]
	double getAnnualPowerApproximation();
	double getDesignThermalPowerWithLoss();
	double getActualThermalPowerWithLoss();
	
	simulation_info *getSimInfoObject();
	simulation_error *getSimErrorObject();
	optical_hash_tree *getOpticalHashTree();

	//-------"SETS"
	/*min/max field radius.. function sets the value in units of [m]. Can be used as follows:
	1) SetMinFieldRadius(_tht, 0.75); 
	2) SetMinFieldRadius(100, 1.0); 
	*/
	void setTowerShadowW(double val);
	void setTowerShadowL(double val);
	void setTowerShadow(double arr[2]);
	void setTowerHeight(double val);
	void setLayoutMethod(int val);
	void setRadialSpacingMethod(int val);
	void setSpacingReset(double val);
	void setInitialSpacing(double val);
	void setDesignPower(double val);
	void setAcceptanceAngle(double minmax[2], bool deg=false);
	void setAimpointStatus(bool state);
	void setSimulatedPowerToReceiver(double val);
	void setHeliostatExtents(double xmax, double xmin, double ymax, double ymin);
	
	void isPowerOptimized(bool val); 		//Vary the power output during optimization to identify optimal level?
	bool isPowerOptimized();
	void isThtOptimized(bool val);		//Vary the tower height during optimization to identify optimal level?
	bool isThtOptimized();
	void isPowerRestricted(bool val);		//Restrict the search range for power output to the indicated limits
	bool isPowerRestricted();
	void isThtRestricted(bool val);		//Restrict the search range for tower height to the indicated limits
	bool isThtRestricted();
	bool isSolarFieldCreated();
	void isOpticalZoning(bool val);
	bool isOpticalZoning();

	//Scripts
	void setDefaults();
	void Create(var_set &V, int var_index = 0);
	void Clean();
	bool ErrCheck();
	void CancelSimulation();
	bool CheckCancelStatus();
	
	bool FieldLayout();	//Master layout method for DELSOL solar field geometries
	static bool PrepareFieldLayout(SolarField &SF, WeatherData &wdata, bool refresh_only=false);	//Field layout preparation call for multithreaded apps
	static bool DoLayout( SolarField *SF, sim_results *results, WeatherData *wdata, int sim_first=-1, int sim_last=-1);
	void ProcessLayoutResults(sim_results *results, int nsim_total);	//Call after simulation for multithreaded apps
	static void AnnualEfficiencySimulation( var_set &vset, SolarField &SF, sim_results &results); //, double *azs, double *zens, double *met);
	static void AnnualEfficiencySimulation( string weather_file, SolarField *SF, sim_results &results); //, double *azs, double *zens, double *met);	//overload
	bool UpdateNeighborList(double lims[4], double zen);
	bool UpdateLayoutGroups(double lims[4]);

	void radialStaggerPositions(vector<Point> &HelPos); //Vector of the possible heliostat locations
	void cornfieldPositions(vector<Point> &HelPos);
	//matrix_t<Heliostat> *locateHeliostats(matrix_t<double> &HPos);		//Given a vector of heliostat positions, determine which to use
	Heliostat *whichTemplate(int method, Point &pos);		//Function returning a pointer to the template to use
	void TemplateRange(int pos_order, int method, double *rrange, double *azrange);
	void RefactorHeliostatImages();
	void Simulate(double args[], int nargs=8, bool is_layout = false);		//Method to simulate the performance of the field
	bool SimulateTime(int hour, int day_of_month, int month, double *args, int nargs);
	//void SimulateTime(string data){ string tdat = data; SimulateTime(tdat); };
	bool SimulateTime(const string &data);
	bool SimulateTime(double sun_elevation, double sun_azimuth, double *args, int nargs);
	static void SimulateHeliostatEfficiency(SolarField *SF, Vect *sunvector, Heliostat *helio, double &dni, double &payfactor, bool is_layout = false);
	double calcShadowBlock(Heliostat *H, Heliostat *HS, int mode);	//Calculate the shadowing or blocking between two heliostats
	void updateAllTrackVectors();	//Macro for calculating corner positions
	void calcHeliostatShadows();	//Macro for calculating heliostat shadows
	void calcAllAimPoints(int method, double args[], int nargs=0);
	int getActiveReceiverCount();
	static bool parseHeliostatXYZFile(const std::string &filedat, layout_shell &layout );
	int calcNumRequiredSimulations();
	//double calcLandArea();
	double calcPipingHeatLoss();
	double calcTotalHeatLoss();
	void HermiteFluxSimulation(Hvector &helios, bool keep_existing_profile = false);
	void AnalyticalFluxSimulation(Hvector &helios);
	void CalcDimensionalFluxProfiles(Hvector &helios);
    bool CalcDesignPtSunPosition(int sun_loc_des, double &az_des, double &zen_des);

 } ;

#endif
