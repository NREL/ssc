#ifndef _RECEIVER_H_
#define _RECEIVER_H_ 1


#include <vector>
#include "Toolbox.h"
#include "mod_base.h"


using namespace std;



//Declare referenced classes
class Receiver;
class SolarField;


/*Define a structure that contains information on each mesh point for the receiver surface*/
struct FluxPoint {
	Point location;
	Vect normal;
	double 
		/*x, //[m] Node x-position (-East, +West) in global coordinates
		y, //[m] Node y-position (-South, +North) in global coordinates
		z, //[m] Node z-position (+vertical) relative to receiver optical height
		i,	//Normal vector to the point, i direction
		j,	//Normal vector to the point, j direction
		k,	//Normal vector to the point, k direction*/
		maxflux, //[W/m2] Maximum allowable flux on this element
		flux, //[W/m2] Actual flux on this element
		area_factor;	//[0..1] Factor adjusting the area of the point to accommodate edge effects
	bool over_flux;		//Flag indicating whether this element has exceeded its flux limit
	
	FluxPoint();
	
	void Setup(double xloc, double yloc, double zloc, Vect &norm, double flux_max, double Area_factor = 1.);
	void Setup(Point &loc, Vect &norm, double flux_max, double Area_factor = 1.);

};

typedef vector<vector<FluxPoint>> FluxGrid;

//The FluxSurface class
class FluxSurface : public mod_base
{
	/* 
	The FluxSurface class provides a data structure that describes in detail the surface on which 
	flux from the heliostat field will be collected. The class is a set of points with associated 
	normal vectors describing their location in x,y,z space and a unit vector that is normal to 
	that point which indicates the orientation of the absorber surface.

	The class contains methods to develop the flux surface from standard receiver geometries as 
	described in DELSOL.
	*/

	int 
		_id, //Absorber surface unique ID
		_type, //Absorber surface type
		/* 
		Types:
		0 | Continuous closed cylinder - external
		1 | Continuous open cylinder - external
		2 | Continuous open cylinder - internal cavity
		3 | Planar rectangle
		4 | Planar ellipse
		5 | Discrete closed N-polygon - external
		6 | Discrete open N-polygon - external
		7 | Discrete open N-polygon - internal cavity
		*/
		_nflux_x,	//Number of flux points horizontally
		_nflux_y;	//Number of flux points vertically
	double
		_width,	
		_height,
		_radius,	//only applies to curved surfaces. If none, should be 0.
		_area,
		_span_ccw,
		_span_cw,
		_max_flux;	//Maximum allowable flux on this surface
	double _max_observed_flux;

	Vect
		_normal;
	Point
		_offset;

	FluxGrid _flux_grid; // Vector containing grid

	Receiver *_rec_parent;

public:
	//----Access functions
	Receiver *getParent();
	int getId();
	FluxGrid *getFluxMap();
	int getFluxNX();
	int getFluxNY();
	Point *getSurfaceOffset();
	double getSurfaceWidth();
	double getSurfaceHeight();
	double getSurfaceRadius();
	double getSurfaceArea();
	double getTotalFlux();
	double getMaxObservedFlux();

	void setParent(Receiver *recptr);
	void setFluxPrecision(int nx, int ny);
	void setMaxFlux(double maxflux);
	void setNormalVector(Vect &vect);
	void setSurfaceOffset(Point &loc);
	void setSurfaceSpanAngle(double span_min, double span_max);
	void setSurfaceGeometry(double height, double width, double radius = 0.);
	void setMaxObservedFlux(double fmax);
	
	//Declare the scripts
	void DefineFluxPoints(int rec_geom=-1, int nx=-1, int ny=-1);
	void Normalize();
	void Reshape(int nx, int ny);
	void ClearFluxGrid();
};

typedef vector<FluxSurface> FluxSurfaces;


//Create the main receiver class
class Receiver : public mod_base
 {

	double
		_span_min,	//Minimum (CCW) bound of the arc defining the receiver surface
		_span_max,	//Maximum (CW) bound of the arc defining the receiver surface
		_panel_rotation,	//Azimuth angle between the normal vector to the primary "north" panel and North
		_width_opt_max,	//Maximum receiver width during optimization
		_width_opt_min,	//Minimum receiver width during optimization
		_diam_opt_max,	//Maximum receiver diameter during optimization
		_diam_opt_min,	//Minimum receiver diameter during optimization
		_height_opt_max,	//Maximum receiver height during optimization
		_height_opt_min,	//Minimum receiver height during optimization
		_h,				//Height of the absorbing component
		_rec_aspect,	//Ratio of receiver height to width
		_d,				//Receiver diameter for cylindrical receivers
		_w,				//Receiver width for cavity or flat receivers
		_rec_az,	//Receiver azimuth orientation: 0 deg is north, positive clockwise
		_rec_elevation,	//Receiver elevation orientation: 0 deg to the horizon, negative rotating downward
		_rec_cav_rad,	//Radius of the receiver cavity absorbing surface
		_rec_cav_cdepth,	//Offset of centroid of cavity absorber surface from the aperture plane. (Positive->Increased depth)
		_absorber_area,	//Effective area of the receiver absorber panels
		_opt_height,	//Calculated height of the centerline of the receiver above the plane of the heliostats
		_rec_offset_x,	//Offset of receiver center in the East(+)/West(-) direction from the tower
		_rec_offset_y,	//Offset of receiver center in the North(+)/South(-) direction from the tower
		_rec_offset_z,	//Offset of the receiver center in the vertical direction, positive upwards
		_peak_flux,	//Maximum allowable flux intensity on any portion of the receiver surface
		_absorptance,	//Energy absorbed by the receiver surface before accounting for radiation/convection losses
		_therm_loss_base,	//Thermal loss from the receiver at design-point conditions
		_therm_loss,	//Receiver thermal loss at design
		_piping_loss_coef,	//Loss per meter of tower height
		_piping_loss_const,	//Constant thermal loss due to piping - doesn't scale with tower height
		_piping_loss,	//Thermal loss from non-absorber receiver piping
		_accept_ang_x,		//Acceptance angle of the receiver in the horizontal direction (in aperture coordinates)
		_accept_ang_y;		//Acceptance angle of the receiver in the vertical direction (in aperture coordinates)

		
	bool
		_is_width_opt, 		//Optimize aperture width
		_is_diam_opt, 		//Optimize receiver diameter
		_is_height_opt, 		//Optimize receiver height
		_is_width_restrict, 		//Restrict aperture width range
		_is_diam_restrict, 		//Restrict receiver diameter range
		_is_height_restrict,		//Restrict receiver height range
		_is_enabled,		//Is template enabled?
		_is_open_geom,		//If true, the receiver is represented by an arc rather than a closed circle/polygon
		_is_polygon;		//Receiver geometry is represented as discrete polygon of N panels rather than continuous arc

	double _thermal_eff;	//An estimate of the thermal efficiency

	string 
		_rec_name;		//Receiver template name

	PointVect
		_normal; //Unit vector of the normal to the reciever
	int
		_rec_type, //General receiver type, 0=External cylinder, 1=Cavity, 2=Flat plate
		_rec_geom, //Specific receiver geometry, defined in DefineReceiverGeometry
		_id,	//Template ID
		_n_panels,		//Number of receiver panels (polygon facets) for a polygonal receiver geometry
		_aperture_type,		//The shape of the receiver aperture. 0=Rectangular, 1=Elliptical
		_accept_ang_type;		//Receiver angular acceptance window defines angles about the aperture normal, can be rectangular or elliptical shape

	matrix_t<double>
		_therm_loss_load,		//Load-based thermal loss adjustment	Temperature-dependant thermal loss
		_therm_loss_wind;		//Wind speed-dependant thermal loss

	FluxSurfaces 
		_surfaces; //A vector containing sub-vectors that define the geometry of receiver surfaces

public:	
	//Receiver (){}; // Constructor 
	//~Receiver(){};
	
	void Create(var_map &V);	//create from variable map
	
	/* Define an enumeration structure for receiver geometry types */
	struct REC_GEOM_TYPE { 
		enum A { CYLINDRICAL_CLOSED, CYLINDRICAL_OPEN, CYLINDRICAL_CAV, PLANE_RECT, 
			PLANE_ELLIPSE, POLYGON_CLOSED, POLYGON_OPEN, POLYGON_CAV}; 
	};
	//define an enumeration for receiver base types
	struct REC_TYPE {
		enum A {CYLINDRICAL, CAVITY, FLAT_PLATE};
	};

	//Declare "GET" access functions
	int getReceiverType(); //[-] Returns receiver type integer (see definitions above)
	int getReceiverGeomType();	//Receiver geometry type, see DefineReceiverGeometry()
	int getReceiverApertureType();	//Receiver aperture type. 0=Rectangular, 1=Elliptical
	int getAcceptAngleType();
	double getReceiverWidth(); //[m] Returns either receiver width or diameter, depending on configuration
	double getReceiverHeight(); //[m]  Returns receiver height
	double getReceiverAzimuth();	//[rad] Returns receiver azimuth {0 = North}
	double getReceiverElevation();  //[rad] returns receiver zenith
	double getOffsetX();
	double getOffsetY();
	double getOffsetZ();
	double getOpticalHeight();	//[m] Optical height
	double getAbsorptance();
	double getReceiverAbsorberArea();
	double getReceiverThermalLoss();
	double getReceiverPipingLoss();
	double getReceiverThermalEfficiency();
	double getNumberPanels();
	double getPanelRotation();
	void getAcceptAngles(double &theta_x, double &theta_y);
	void getReceiverOffset(Point &offset);
	void getReceiverSpan(double &span_min, double &span_max, double &azimuth);
	void CalculateNormalVector(PointVect &NV);	//Returns the normal vector and receiver centroid that represents the optimal optical incidence
	void CalculateNormalVector(Point &Hloc, PointVect &NV);	//(Overload) for non-flat receivers, closest normal vector given a viewpoint vector
	FluxSurfaces *getFluxSurfaces();
	string *getReceiverName();
	bool isReceiverEnabled();
	void isReceiverEnabled(bool enable);

	//Declare "SET" access functions
	bool setReceiverType(const int &val); //Sets receiver type. Must be among -1..4
	bool setOpticalHeight(const double &val); //[m] Sets the optical height of the receiver
	void setReceiverHeight(double &val);
	void setReceiverDiameter(double &val);
	void setReceiverWidth(double &val);
		
	//Declare the scripts
	void setDefaults();
	void DefineReceiverGeometry(int nflux_x = 1, int nflux_y = 1);
	void CalculateAbsorberArea();
	void CalculateThermalLoss(double load, double v_wind);
	void CalculateThermalEfficiency(double dni, double dni_des, double v_wind);
	double CalculateApparentDiameter(Point &Hloc); //[m] Return the apparent receiver diameter given the polygonal structure
 } ;

#endif