#ifndef _HELIOSTAT_H_
#define _HELIOSTAT_H_ 1
#include "math.h"
//#include "SolarField.h"
//#include "Flux.h"
#include "heliodata.h"
#include "mod_base.h"
#include <vector>
using namespace std;
//using namespace util;

//declare derived classes referenced in the heliostat class
class Flux;
class Reflector;
class SolarField;
class Receiver;
class Ambient;

class Heliostat : public mod_base
 {

	Point
		_location, //Location of heliostat in the field (0,0,Zt) is the location of the tower base
		_aim_point,	//constant aim point for the heliostat
		_aim_fluxplane;	//Aim point with respect to the flux plane
	Vect
		_track, //The tracking vector for the heliostat
		_tower_vect,  //Heliostat-to-tower unit vector
		_cant_vect;	//Canting vector (not normalized)
	vector<Heliostat*>
		*_neighbors; //pointer to a vector of neighboring heliostats
	matrix_t<Reflector>
		_panels; //Array of cant panels
	vector<Point>
		_corners,	//Position in global coordinates of the heliostat corners (used for blocking and shadowing)
		_shadow;	//Position in global coordinates of the heliostat shadow
	Heliostat* _master_template;	//Pointer to the template used to create this heliostat
		
	matrix_t<double>
		_mu_MN,		//Normalized mirror shape hermite expansion coefficients (n_terms x n_terms)
		_mu_S,		//Moments of sunshape
		_mu_G,		//Moments of the error distribution
		_mu_M,		//Moments of the mirror shape
		_mu_F,		//Flux moments distrubution - result
		_hcoef,		//Hermite coefficients
		_hc_tht; 		//Hermite coefs depending on tower height - equiv. to mu_F, reused in optimization calcs

	bool
		_in_layout, // Is the heliostat included in the final layout?
		_is_round, // Is the heliostat round (true) or rectangular (false)
		_is_focal_equal, //Both the X and Y focal lengths will use a single value as indicated by the X focal length
		_is_xfocus, // Is the reflector focused w/r/t the x-axis?
		_is_yfocus, // Is the reflector focused w/r/t the y-axis?
		_is_user_canted,	//Are the panels canted according to user-specified values?
		_is_cant_vect_slant,	//Multiply the canting vector by the slant range
		_is_cant_rad_scaled,	//The cant radius scales with tower height
		_is_faceted,		//The number of reflective panels per heliostat is greater than 1
		_is_enabled;		//Is template enabled?


	int 
		_id, //Unique ID number for the heliostat/surface
		_type,	//Integer used to group heliostats into geometries within a field, (e.g. 5 different focal length designs)
		_group[2],	//Integers {row,col} used to determine which local group the heliostat is in. 
		_cant_method,	//Integer to specify the canting method {0=none, -1=Cant on-axis equal to slant range, 1=user-defined on-axis, 3=user-defined off-axis at hour + day}
		_focus_method,		//The focusing method {0=Flat, 1=Each at slant, 2=Average of group, 3=User defined}
		_ncantx, //Number of cant panels in the X direction
		_ncanty, //Number of cant panels in the Y direction
		_nrotax; //Number of rotational axes on the tracker (1 for LFresnel, 2 for heliostat)
	double
		_temp_rad_min,		//Minimum radius at which this heliostat geometry can be used
		_temp_rad_max,		//Maximum radius at which this heliostat geometry can be used
		_temp_az_min, 		//Angular boundary for heliostat geometry - on the counter-clockwise side of the region
		_temp_az_max,	//Angular boundary for heliostat geometry - on the clockwise side of the region
		_wm, //[m] Width of the heliostat structure
		_hm, //[m] height of the heliostat structure
		_wgap,	//[m] Separation between panels in the horizontal direction
		_hgap,	//[m] Separation between panels in the vertical direction
		_hcant, //[hr] Mirror panels are canted at 'hcant' hours past noon on...
		_dcant, //[day] ...'dcant' day of the year.
		_rcant, //[m] Radius for canting focal point assuming on-axis canting
		_densmr, //[-] Ratio of mirror area to total area of the heliostat defined by wm x hm
		_sigel, //[rad] Standard deviation of the normal error dist. of the elevation angle
		_sigaz, //[rad] Standard deviation of the normal error dist. of the azimuth angle
		_sigsx, //[rad] Std.dev. of the normal error dist. of the reflective surface normal in the X (horizontal)
		_sigsy, //[rad] Same as above, but in the vertical direction
		_sigtx, //[rad] error in reflected vector (horiz.) caused by atmospheric refraction, tower sway, etc.
		_sigty, //[rad] error in reflected vector (vert.) caused by atmospheric refraction, tower sway, etc.
		_xfocal, //[m] focal length in the i^ direction
		_yfocal, //[m] focal length in the j^ direction
		_maxrvelx, //[rad/s] maximum rotational velocity about the x axis
		_maxrvelz, //[rad/s] maximum rotational velocity about the z axis
		_rcoll, //[m] Collision avoidance radius
		_area, //[m2] reflective aperture area
		_dm,	//Diameter of the heliostat structure (round heliostats only)
		_rcant_scaled,	//Canting radius - scaled by tower height
		_cant_vect_scale;	//Value to scale the canting unit vector to determine actual canting magnitude

	helio_perf_data
		eff_data;
	double
		_slant,		//[m] Heliostat slant range - path length from centroid to receiver aim point
		_zenith,	//[rad] Heliostat tracking zenith angle
		_azimuth,	//[rad] Heliostat tracking azimuth angle
		_image_size_xy[2];	//[m/m] Image size on the receiver plane in {x,y}, normalized by tower height
	string
		_helio_name;		//Heliostat template name
	Receiver *_which_rec;	//Which of the receivers is the heliostat pointing at?
		
public:
	//constructor and destructor
	/*Heliostat(){};
	~Heliostat(){};*/

	//Declare other subroutines
	void installPanels();	//Define the cant panel locations, pointing vectors, and shape
	void setDefaults();
	void Create(var_map &V);
	void updateTrackVector(Vect &sunvect);	//Update the tracking vector for the heliostat
	double calcTotalEfficiency();
	void resetMetrics();
	void CopyImageData(const Heliostat *Hsrc);

	//Declare access functions
	int getId();
	int *getGroupId();		//(row,col) nodes
	bool getInLayout();
	double getWidth();
	double getHeight();
	double getCollisionRadius();
	double getWGap(); //[m] Gap in the horizontal direction (width)
	double getHGap(); //[m] Gap in the vertical direction
	double getArea();
	double getReflectiveAreaDerate();
	double getFocalX();
	double getFocalY();
	double getSlantRange();
	double getCantRadius();
	double getCantDay();
	double getCantHour();
	int getNumCantY();
	int getNumCantX();
	int getCantMethod();
	int getFocusMethod();
	Receiver *getWhichReceiver();
	int getType();
	double getRadialPos();
	double getAzimuthalPos();
	Reflector *getPanelById(int id);
	Reflector *getPanel(int row, int col);
	matrix_t<Reflector> *getPanels();
	Vect *getTrackVector();	//return the tracking vector
	Vect *getTowerVector(); // return the helio-tower unit vector
	Vect *getCantVector();	//Return the canting vector (not normalized)
	Point *getLocation(); //Get location vector
	Point *getAimPoint();	//Get the heliostat aim point on the receiver
	Point *getAimPointFluxPlane();	//aim point in the flux plane coordinates 
	void getErrorAngular(double err[2]);
	void getErrorSurface(double err[2]);
	void getErrorReflected(double err[2]);
	helio_perf_data *getEfficiencyObject();
	double getTotalReflectivity();
	double getEfficiencyTotal();
	double getEfficiencyCosine();
	double getEfficiencyAtten();
	double getEfficiencyIntercept();
	double getEfficiencyBlock();
	double getEfficiencyShading();
	double getEfficiencyCloudiness();
	double getPowerToReceiver();
	double getPowerValue();
	double getRankingMetricValue();
	double getAzimuthTrack();
	double getZenithTrack();
	vector<Heliostat*> *getNeighborList();
	vector<Point> *getCornerCoords();
	vector<Point> *getShadowCoords();
	matrix_t<double> *getMirrorShapeNormCoefObject();
	matrix_t<double> *getMirrorShapeCoefObject();
	matrix_t<double> *getSunShapeCoefObject();
	matrix_t<double> *getErrorDistCoefObject();
	matrix_t<double> *getFluxMomentsObject();
	matrix_t<double> *getHermiteCoefObject();
	matrix_t<double> *getHermiteNormCoefObject();
	double *getImageSize();
	void getImageSize(double &sigx_n, double &sigy_n);
	string *getHeliostatName();
	void getSummaryResults( vector<double> &results );
	Heliostat* getMasterTemplate();
	void getTemplateRange(double &rmin, double &rmax, double &azmin, double &azmax);

	bool IsRound();	//Fetch
	void IsRound(bool setting);
	bool IsUserCant(); //Fetch
	void IsUserCant(bool setting);	//Set
	bool IsUserFocus();
	void IsUserFocus(bool setting);
	bool IsFacetDetail();	//Is characterization of multiple facets required?
	void IsFacetDetail(bool setting);

	void setId(int id);
	void setGroupId(int row, int col);
	void setType(int type);
	void setInLayout(bool in_layout);
	void setNeighborList(vector<Heliostat*> *list);
	void setWidth(double &val, bool update_cant = false);
	void setHeight(double &val, bool update_cant = false);
	void setDiameter(double &val, bool update_cant = false);
	void setEfficiencyCosine(double eta_cos);
	void setEfficiencyAtmAtten(double eta_att);
	void setEfficiencyIntercept(double eta_int);
	void setEfficiencyBlocking(double eta_block);
	void setEfficiencyShading(double eta_shadow);
	void setEfficiencyCloudiness(double eta_cloud);
	void setEfficiencyTotal(double eta_tot);
	void setRankingMetricValue(double rval);
	void setLocation(double x, double y, double z);
	void setAimPoint(double x, double y, double z);
	void setAimPoint(Point &Aim);
	void setAimPointFluxPlane(Point &Aim);
	void setAimPointFluxPlane(double x, double y, double z);
	void setTrackVector(Vect &tr);	//Set the tracking vector
	void setTowerVector(Vect &tow); //Set the helio-tower vector
	void setTrackAngleZenith(double zenith);
	void setTrackAngleAzimuth(double azimuth);
	void setTrackAngles(double azimuth, double zenith);
	void setCantMethod(int method);
	void setCantVector(Vect &cant);
	void setCantVector(double cant[3]);
	void setFocusMethod(int method);
	void setSlantRange(double L);
	void setFocalLengthX(double L);
	void setFocalLengthY(double L);
	void setFocalLength(double L);
	void setCantRadius(double L);
	void setErrorAngular(matrix_t<double> &E);
	void setErrorAngular(double sigaz, double sigel);
	void setErrorSurface(matrix_t<double> &E);
	void setErrorSurface(double sigsx, double sigsy);
	void setErrorReflected(matrix_t<double> &E);
	void setErrorReflected(double sigtx, double sigty);
	//void setWhichReceiver(int rec){_which_rec = rec;}
	void setWhichReceiver(Receiver *rec);
	void setPowerToReceiver(double P);
	void setPowerValue(double P);
	void setImageSize(double sigx_n, double sigy_n);
	void setMasterTemplate(Heliostat *htemp);
 } ;

class Reflector {
	double 
		_width, //[m] reflector width (type=1)
		_height, //[m] reflector height (type=1)
		_diameter, //[m] reflector diameter (type=2)
		_focal_length; //[m] Focal distance, (if < 0, assume infinite focus)
	int 
		_id,	//Unique identifier for this reflector within a larger group
		_type;	//Which type of reflector (1=rectangular, 2=circular, 3=user-defined)
	matrix_t<PointVect>
		_geometry;	//A vector of points w/ orientation that define the outline of the geometry (type=3)
	PointVect
		_locate_vector;	//PointVect that locates and aims the reflector relative to it's frame of reference, (type=1,2)

public:
	//Constructors
	Reflector();
	//Reflector(Reflector &R);
	
	//Get-Set methods
	int getId();
	double getWidth();
	double getHeight();
	double getDiameter();
	double getFocalLength();
	int getType();
	PointVect *getOrientation();
	

	void setId(int id);
	void setType(int type);
	void setWidth(double width);
	void setHeight(double height);
	void setDiameter(double diam);
	void setPosition(double x, double y, double z);
	void setAim(double i, double j, double k);
	void setAim( Vect &V);
	void setOrientation(double x, double y, double z, double i, double j, double k);
	void setOrientation(PointVect &PV);
	
	//Define functions
	void setDefaults();
};


class HelioTemplate : public Heliostat {
	int 
		_n_focal_types;	//How many different focal lengths can there be? (-1=Inf, 1,2,..)
public:
	HelioTemplate(){};
	~HelioTemplate(){};

	//Add methods here to help the layout tool decide which heliostat to use.
};


#endif