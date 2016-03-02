#include "Receiver.h"
#include <math.h>
#include "exceptions.hpp"
#include <vector>
using namespace std;

//----------------FluxPoint -----------------
FluxPoint::FluxPoint(){over_flux = false; flux = 0.;};
	
void FluxPoint::Setup(double xloc, double yloc, double zloc, Vect &norm, double flux_max, double Area_factor){
	location.x = xloc; location.y = yloc; location.z = zloc;
	normal.i = norm.i; normal.j = norm.j; normal.k = norm.k;
	maxflux = flux_max;
	over_flux = false;
	area_factor = Area_factor;
};
void FluxPoint::Setup(Point &loc, Vect &norm, double flux_max, double Area_factor){
	location.x = loc.x; location.y = loc.y; location.z = loc.z;
	normal.i = norm.i; normal.j = norm.j; normal.k = norm.k;
	maxflux = flux_max;
	over_flux = false;
	area_factor = Area_factor;
}


//----------------Absorber surface-----------------
Receiver *FluxSurface::getParent(){return _rec_parent;}
int FluxSurface::getId() {return _id;};
FluxGrid *FluxSurface::getFluxMap(){return &_flux_grid;}
int FluxSurface::getFluxNX(){return _nflux_x;}
int FluxSurface::getFluxNY(){return _nflux_y;}
Point *FluxSurface::getSurfaceOffset(){return &_offset;}
double FluxSurface::getSurfaceWidth(){return _width;}
double FluxSurface::getSurfaceHeight(){return _height;}
double FluxSurface::getSurfaceRadius(){return _radius;}
double FluxSurface::getSurfaceArea(){return _area;}
double FluxSurface::getMaxObservedFlux(){return _max_observed_flux;}

void FluxSurface::setParent(Receiver *recptr){_rec_parent = recptr;}
void FluxSurface::setFluxPrecision(int nx, int ny){_nflux_x=nx; _nflux_y = ny;}
void FluxSurface::setMaxFlux(double maxflux){_max_flux = maxflux;}
void FluxSurface::setNormalVector(Vect &vect){
	_normal = vect;
}
void FluxSurface::setSurfaceOffset(Point &loc){_offset = loc;}
void FluxSurface::setSurfaceSpanAngle(double span_min, double span_max){_span_ccw = span_min; _span_cw = span_max;}
void FluxSurface::setSurfaceGeometry(double height, double width, double radius){
	_width = width;
	_height = height;
	_radius = radius;	//if radius is 0, assume flat surface.
						//For nonzero radii, the WIDTH WILL BE
}
void FluxSurface::setMaxObservedFlux(double fmax){ _max_observed_flux = fmax; }
	
//Declare the scripts
void FluxSurface::ClearFluxGrid(){
	for(unsigned int i=0; i<_flux_grid.size(); i++){
		for(unsigned int j=0; j<_flux_grid.at(i).size(); j++){
			_flux_grid.at(i).at(j).flux = 0.;
		}
	}
}

void FluxSurface::DefineFluxPoints(int rec_geom, int nx, int ny){
	/*
	Given the receiver geometry in "_parent", create a grid of flux hit test points.

	Flux points are in the global coordinate system but do not include receiver offset or tower height.

    */
	if(nx > 0) _nflux_x = nx;
	if(ny > 0) _nflux_y = ny;

	if(rec_geom > -1) _type = rec_geom;	//Use the argument. Otherwise, use the locally set value

	switch (rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
	{			//0 | Continuous closed cylinder - external
		
		//The flux for this geometry assumes that the cylinder is vertical (no zenith displacement)
        //Flux points are stored beginning lower edge, clockwise extent. Final entry upper edge counterclockwise extent

		_area = _height * _radius * pi * 2.;

		//Resize
		_flux_grid.resize(_nflux_x);	//number of rows

		//The azimuthal span taken up by each flux point
		double daz = (_span_cw - _span_ccw)/double(_nflux_x);	

		double faz;
		Point floc;
		Vect fnorm;
		double dz = _height/double(_nflux_y);	//height of each flux node
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

            faz = _span_cw - daz*(0.5 + (double)i); //The azimuth angle of the point
			//Calculate the normal vector
			fnorm.i = sin(faz);
			fnorm.j = cos(faz);
			fnorm.k = 0.;
			//Calculate the x-y position
			floc.x = fnorm.i * _radius;
			floc.y = fnorm.j * _radius;
			//Calculate the z position
			for(int j=0; j<_nflux_y; j++){
				floc.z = -_height/2.+dz*(0.5 + (double)j);
				//Set the location
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
	{		
		//1 | Continuous open cylinder - external
		//2 | Continuous open cylinder - internal cavity

		/*
		The flux map for this geometry allows an angling in the zenith direction of the surface.
		The coordinates of the flux map are with respect to the xyz location of the receiver centroid.
		These coordinates account for zenith rotation of the receiver. 
        
        Flux points are stored beginning lower edge, clockwise extent. Final entry upper edge counterclockwise extent
		
        */
        
        double intmult = ( rec_geom == Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV ? -1. : 1. );   //-1 multiplier for values that are inverted on the internal face of a cylinder
        double spansize = (_span_cw - _span_ccw) * intmult;

		_area = _height * _radius * spansize;

		//Resize
		_flux_grid.resize(_nflux_x); //Number of rows
		
		//The azimuthal span taken up by each point
		double daz = spansize/double(_nflux_x)  ;
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);

		double faz, fzen;
		Point floc;
		Vect fnorm;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

			faz = _span_cw - daz*(0.5+double(i));
			fzen = rec_zen*cos(rec_az - faz);	//Local receiver zenith angle
			for(int j=0; j<_nflux_y; j++){
				//Calculate the xyz position assuming no rotation, then rotate into the position of the receiver
				floc.x = _radius*sin(faz);
				floc.y = _radius*cos(faz);
				floc.z = -_height/2.+rec_dh*(0.5 + double(j));
				//Calculate the normal vector
				fnorm.i = sin(faz)*cos(fzen)*intmult;
				fnorm.j = cos(faz)*cos(fzen)*intmult;
				fnorm.k = sin(fzen);

				//rotate about the x axis (zenith)
				Toolbox::rotation(rec_zen, 0, floc);	//Rotate the actual point
				Toolbox::rotation(rec_zen, 0, fnorm);	//rotate the normal vector
				//rotate about the z axis (azimuth)
				Toolbox::rotation(rec_az, 2, floc);     //point
                Toolbox::rotation(rec_az, 2, fnorm);    //normal vector

				//the point "floc" is now rotated into the receiver coordinates
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
	{		//3 | Planar rectangle
		/* 
		The receiver is a rectangle divided into _nflux_x nodes in the horizontal direction and
		_nflux_y nodes in the vertical direction. Each node is of area A_rec/(_nflux_x * _nflux_y).
		*/

		_area = _height * _width;

		_flux_grid.resize(_nflux_x); //Number of rows
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);
		double rec_dw = _width/double(_nflux_x);

		Point floc;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns
			for(int j=0; j<_nflux_y; j++){
				//Calculate the position assuming no rotation, then rotate according to the receiver orientation
				floc.x = (-_width + rec_dw)/2. + i*rec_dw;
				floc.y = (-_height + rec_dh)/2. + j*rec_dh;
				floc.z = 0.;
				
				//Rotate about the x axis (zenith)
				Toolbox::rotation(-rec_zen, 0, floc);
				//Rotate about the z axis (azimuth)
				Toolbox::rotation(pi + rec_az, 2, floc);

				//Set up the point
				_flux_grid.at(i).at(j).Setup(floc, _normal, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
	{		//4 | Planar ellipse
		/* 
		The receiver is a rectangle divided into _nflux_x nodes in the horizontal direction and
		_nflux_y nodes in the vertical direction. Each node is of area A_rec/(_nflux_x * _nflux_y).
		*/

		_area = pi * _width * _height/4.;

		_flux_grid.resize(_nflux_x); //Number of rows
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);
		double rec_dw = _width/double(_nflux_x);
		
		Point floc;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns
			for(int j=0; j<_nflux_y; j++){
				//Calculate the position assuming no rotation, then rotate according to the receiver orientation
				floc.x = (-_width + rec_dw)/2. + i*rec_dw;
				floc.y = 0.;
				floc.z = (-_height + rec_dh)/2. + j*rec_dh;
				
				//Calculate an area factor to account for incongruence of the rectangular node with the elliptical aperture
				double rect[] = {floc.x, floc.z, rec_dw, rec_dh};
				double ellipse[] = {_width, _height};
				double afactor = fmin(fmax(Toolbox::intersect_ellipse_rect(rect, ellipse)/(rec_dw*rec_dh), 0.), 1.);

				//Rotate about the x axis (zenith)
				Toolbox::rotation(-rec_zen + pi/2., 0, floc);       //unlike plane rect, the points start in X-Z plane
				//Rotate about the z axis (azimuth)
				Toolbox::rotation(pi + rec_az, 2, floc);

				//Set up the point
				_flux_grid.at(i).at(j).Setup(floc, _normal, _max_flux, afactor);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
	{
		
		//The flux for this geometry assumes that the cylinder is vertical (no zenith displacement)

		_area = _height * _radius * pi * 2.;

		//Resize
		_flux_grid.resize(_nflux_x);	//number of rows

		//The azimuthal span taken up by each flux point
		double span = (_span_cw - _span_ccw);
		double daz = span/double(_nflux_x);	//span will always be 2 pi for this

		int npanels = _rec_parent->getNumberPanels();
		double panel_az = _rec_parent->getPanelRotation();
		//calculate the angular span each panel occupies
		double panel_az_span = span / (double)npanels;
		
		//pre-calculate normal vectors for each of the panels
		vector<Vect> panel_normals(npanels);
		vector<double> panel_radii(npanels);
		vector<double> panel_azimuths(npanels);
		
        double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		
		for(int i=0; i<npanels; i++){
			double paz = _span_cw - (i + 0.5)*panel_az_span;
			double 
				sinpaz = sin(paz),
				cospaz = cos(paz);
			panel_normals.at(i).Set(sinpaz, cospaz, 0.);
			panel_radii.at(i) = _radius*cos(panel_az_span/2.) ;
			panel_azimuths.at(i) = paz + rec_az;

            //rotate panels according to receiver elevation/azimuth
            Toolbox::rotation(rec_zen + pi/2., 0, panel_normals.at(i));
            Toolbox::rotation(rec_az, 2, panel_normals.at(i));

		}

		double faz;
		Vect fnorm;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

			faz = _span_cw - daz*(0.5 + double(i)) + rec_az;	//The azimuth angle of the point

			//which panel does this flux point belong to?
			int ipanl = (int)(floor(faz / panel_az_span));

			//the normal vector is the same as the panel to which it belongs
			fnorm.Set(panel_normals.at(ipanl));
			
			//Determine the flux point position, which must lie along an existing panel
			
			double h = panel_radii.at(i)/cos( panel_azimuths.at(i) - faz ); //hypotenuse 

			//x-y location of flux point
			Point floc;
			floc.x = h * sin(faz - rec_az);
			floc.y = h * cos(faz - rec_az);

			//Calculate the z position
			double dz = _height/double(_nflux_y);	//height of each flux node
			for(int j=0; j<_nflux_y; j++){
				floc.z = -_height/2.+dz*(0.5 + double(j));

                // rotate
                Toolbox::rotation(rec_zen + pi/2., 0, floc);
                Toolbox::rotation(rec_az, 2, floc);

				//Set the location
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::POLYGON_OPEN:
	case Receiver::REC_GEOM_TYPE::POLYGON_CAV:
	default:
		break;
	}

}

double FluxSurface::getTotalFlux(){
	//Determine the total flux on the surface
	double flux_tot=0.;
	for(int i=0; i<_nflux_x; i++){
		for(int j=0; j<_nflux_y; j++){
			flux_tot += _flux_grid.at(i).at(j).flux;
		}
	}
	return flux_tot;
}

void FluxSurface::Normalize(){
	/* 
	Express each node on the flux map as a relative contribution toward the total 
	absorbed flux, which is equal to 1.0. 
	e.g:
	sum_i=0->nfx( sum_j=0->nfy( flux[i][j] )) = 1.0
	*/

	double flux_tot = getTotalFlux();

	//Normalize by the total
	for(int i=0; i<_nflux_x; i++){
		for(int j=0; j<_nflux_y; j++){
			_flux_grid.at(i).at(j).flux *= 1./flux_tot;
		}
	}

}

void FluxSurface::Reshape(int nx, int ny){
	/* 
	Take the current flux map and shape it into the specified dimension while maintaining total power
	*/
	double flux_tot = getTotalFlux();

	int
		index_start = 0,
		index_now = 0,
		nx_old = _nflux_x,
		ny_old = _nflux_y;

	double
		ifact_x = (float)nx/(float)nx_old,
		ifact_y = (float)ny/(float)ny_old;

	//FluxGrid grid_temp(nx,ny);

	//Code Here -- TO DO


}


//-----------------Receiver----------------

void Receiver::setDefaults(){
	var_map V;	//empty variable map, uses defaults
	Create(V);
}

void Receiver::Create(var_map &V)
{
	//Sets the local variables to their defaults (mostly from DELSOL)
	setVar("rec_name", _rec_name, V, "Receiver 1");		//Receiver template name
	setVar("id", _id, V, -1, "[-1,9e99]");		//Template ID
	setVar("is_enabled", _is_enabled, V, true);		//Is template enabled?
	setVar("rec_type", _rec_type, V, 0, "[0,5]");		//Receiver geometrical configuration
	setVar("is_open_geom", _is_open_geom, V, false);		//If true, the receiver is represented by an arc rather than a closed circle/polygon
	setVar("is_polygon", _is_polygon, V, false);		//Receiver geometry is represented as discrete polygon of N panels rather than continuous arc
	setVar("is_aspect_opt", _is_aspect_opt, V, true);		//Optimize receiver aspect ratio (height / width)
	setVar("is_aspect_restrict", _is_aspect_restrict, V, true);		//Restrict aspect ratio range (height / width)
	setVar("aspect_opt_max", _aspect_opt_max, V, 2., "[0.,1000.]");		//Maximum receiver aspect ratio during optimization
	setVar("aspect_opt_min", _aspect_opt_min, V, 0.5, "[0.,1000.]");		//Minimum receiver aspect ratio during optimization
	setVar("is_height_opt", _is_height_opt, V, false);		//Optimize receiver height
	setVar("is_height_restrict", _is_height_restrict, V, false);		//Restrict receiver height range
	setVar("height_opt_max", _height_opt_max, V, 30., "[0.,1000.]");		//Maximum receiver height during optimization
	setVar("height_opt_min", _height_opt_min, V, 5., "[0.,1000.]");		//Minimum receiver height during optimization
	setVar("aperture_type", _aperture_type, V, 0);		//The shape of the receiver aperture
	setVar("span_min", _span_min, V, -180., "[-180.,180.]");		//Minimum (CCW) bound of the arc defining the receiver surface
	setVar("span_max", _span_max, V, 180., "[-180.,180.]");		//Maximum (CW) bound of the arc defining the receiver surface
	setVar("panel_rotation", _panel_rotation, V, 0., "[-180.,180.]");		//Azimuth angle between the normal vector to the primary 'north' panel and North
	setVar("rec_height", _h, V, 22., "(0.,1000.]");		//Height of the absorbing component
	setVar("rec_aspect", _rec_aspect, V, 1.2, "(0.,9e9]");		//Ratio of receiver height to width
	setVar("rec_diameter", _d, V, 15.5, "[0.,1000.]");		//Receiver diameter for cylindrical receivers
	setVar("rec_width", _w, V, 15.5, "[0.,1000.]");		//Receiver width for cavity or flat receivers
	setVar("rec_azimuth", _rec_az, V, 0., "[-180.,180]");		//Receiver azimuth orientation: 0 deg is north, positive clockwise
	setVar("rec_elevation", _rec_elevation, V, 0., "[-90.,90.]");		//Receiver elevation orientation: 0 deg to the horizon, negative rotating downward
	setVar("rec_cav_rad", _rec_cav_rad, V, 7.75, "(0.,100]");		//Radius of the receiver cavity absorbing surface
	setVar("rec_cav_cdepth", _rec_cav_cdepth, V, 0., "[-100.,100]");		//Offset of centroid of cavity absorber surface from the aperture plane. (Positive->Increased depth)
	setVar("n_panels", _n_panels, V, 12, "[3,30]");		//Number of receiver panels (polygon facets) for a polygonal receiver geometry
	setVar("absorber_area", _absorber_area, V, 0., "[-9e9,9e9]");		//Effective area of the receiver absorber panels
	setVar("optical_height", _opt_height, V, 180., "(0.,1000.]");		//Calculated height of the centerline of the receiver above the plane of the heliostats
	setVar("rec_offset_x", _rec_offset_x, V, 0., "[-9e9,9e9]");		//Offset of receiver center in the East(+)/West(-) direction from the tower
	setVar("rec_offset_y", _rec_offset_y, V, 0., "[-9e9,9e9]");		//Offset of receiver center in the North(+)/South(-) direction from the tower
	setVar("rec_offset_z", _rec_offset_z, V, 0., "[-9e9,9e9]");		//Offset of the receiver center in the vertical direction, positive upwards
	setVar("peak_flux", _peak_flux, V, 800., "(0.,9e9]");		//Maximum allowable flux intensity on any portion of the receiver surface
	setVar("absorptance", _absorptance, V, 0.94, "(0.,1.]");		//Energy absorbed by the receiver surface before accounting for radiation/convection losses
	setVar("therm_loss_base", _therm_loss_base, V, 30., "[0,9e9]");		//Thermal loss from the receiver at design-point conditions
	setVar("therm_loss_load", _therm_loss_load, V);		//Temperature-dependant thermal loss
	setVar("therm_loss_wind", _therm_loss_wind, V);		//Wind speed-dependant thermal loss
	setVar("therm_loss", _therm_loss, V, 0.);		//Receiver thermal loss at design
	setVar("piping_loss_coef", _piping_loss_coef, V, 10.2, "[0,9e9]");		//Loss per meter of tower height
	setVar("piping_loss_const", _piping_loss_const, V, 0., "[0,9e9]");		//Constant thermal loss due to piping - doesn't scale with tower height
	setVar("piping_loss", _piping_loss, V, 0.);		//Thermal loss from non-absorber receiver piping
	setVar("accept_ang_type", _accept_ang_type, V, 0);		//Receiver angular acceptance window defines angles about the aperture normal, can be rectangular or elliptical shape
	setVar("accept_ang_x", _accept_ang_x, V, 180., "[0,180]");		//Acceptance angle of the receiver in the horizontal direction (in aperture coordinates)
	setVar("accept_ang_y", _accept_ang_y, V, 180., "[0,180]");		//Acceptance angle of the receiver in the vertical direction (in aperture coordinates)

	//Unit conversions
	_span_min *= d2r;
	_span_max *= d2r;
	_panel_rotation *= d2r;
	_rec_az *= d2r;
	_rec_elevation *= d2r;
	_accept_ang_x *= d2r;
	_accept_ang_y *= d2r;
	
	_normal = PointVect(0.,0.,0.,0.,1.,0.); //Unit vector of the normal to the reciever
	
	DefineReceiverGeometry();

	CalculateAbsorberArea();

}

//------------Access functions
int Receiver::getReceiverType() {return _rec_type;}; //[-] Returns receiver type integer (see definitions above)
int Receiver::getReceiverGeomType(){return _rec_geom;}	//Receiver geometry type, see DefineReceiverGeometry()
int Receiver::getReceiverApertureType(){return _aperture_type;}	//Receiver aperture type. 0=Rectangular, 1=Elliptical
int Receiver::getAcceptAngleType(){return _accept_ang_type;}
double Receiver::getReceiverWidth() {if(_rec_type == 0) {return _d;} else {return _w;} } //[m] Returns either receiver width or diameter, depending on configuration
double Receiver::getReceiverHeight() {return _h;} //[m]  Returns receiver height
double Receiver::getReceiverAzimuth() { return _rec_az;}	//[rad] Returns receiver azimuth {0 = North}
double Receiver::getReceiverElevation() { return _rec_elevation;}  //[rad] returns receiver zenith
double Receiver::getOffsetX(){return _rec_offset_x;}
double Receiver::getOffsetY(){return _rec_offset_y;}
double Receiver::getOffsetZ(){return _rec_offset_z;}
double Receiver::getOpticalHeight(){return _opt_height;}	//[m] Optical height
double Receiver::getAbsorptance(){return _absorptance;}
double Receiver::getReceiverAbsorberArea(){return _absorber_area;}
double Receiver::getReceiverThermalLoss(){return _therm_loss;}  //kWt
double Receiver::getReceiverPipingLoss(){return _piping_loss;}  //kWt
double Receiver::getReceiverThermalEfficiency(){return _thermal_eff;}
int Receiver::getNumberPanels(){return _is_polygon ? _n_panels : 0; }
double Receiver::getPanelRotation(){return _panel_rotation; }
void Receiver::getAcceptAngles(double &theta_x, double &theta_y){theta_x = _accept_ang_x; theta_y = _accept_ang_y;};
void Receiver::getReceiverOffset(Point &offset){offset.x = _rec_offset_x; offset.y = _rec_offset_y; offset.z = _rec_offset_z;}
void Receiver::getReceiverSpan(double &span_min, double &span_max, double &azimuth){span_min = _span_min; span_max = _span_max; azimuth = _panel_rotation;};
double Receiver::getReceiverCavityRadius(){ return _rec_cav_rad; };
double Receiver::getReceiverCavityDepth(){ return _rec_cav_cdepth; };
FluxSurfaces *Receiver::getFluxSurfaces(){ return &_surfaces; }
string *Receiver::getReceiverName(){return &_rec_name;}
bool Receiver::isReceiverEnabled(){return _is_enabled;}
void Receiver::isReceiverEnabled(bool enable){_is_enabled = enable; }

//Declare "SET" access functions
bool Receiver::setReceiverType(const int &val) {if (val<-1 || val >4){return false;} else {_rec_type = val; return true;}}; //Sets receiver type. Must be among -1..4
bool Receiver::setOpticalHeight(const double &val) {if(val<=0.0 || val>1000.0){return false;} else { _opt_height = val; return true;}}; //[m] Sets the optical height of the receiver
void Receiver::setReceiverHeight(double &val){_h = val;}
void Receiver::setReceiverDiameter(double &val){_d = val;}
void Receiver::setReceiverWidth(double &val){_w = val;}

void Receiver::CalculateNormalVector(PointVect &NV){
	//If no normal vector is supplied, provide the default
	Point V;
	V.Set(0., 0., 0.);
	Receiver::CalculateNormalVector(V, NV);
}

void Receiver::CalculateNormalVector(Point &Hloc, PointVect &NV){
	/* 
	This subroutine should be used to calculate the normal vector to the receiver for a given heliostat location.
	Ultimately, the optical calculations should not use this method to calculate the normal vector. Instead, use
	the normal vector that is assigned to the receiver surface during setup. 

	In the case of continuous cylindrical surfaces, this method can be called during optical calculations.

	Given a heliostat at point Hloc{x,y,z}, return a normal vector to the receiver absorber surface.
	
	*/
	
	//This will have to be modified to allow for multi-surface receivers and polygons. TO DO
	switch(_rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:

		//External cylinder
		//use the view vector to determine the best normal to report
		
		//Polar coords for azimuth and zenith angles
		double vaz; 
		vaz = atan2(Hloc.x,Hloc.y);
		
		//What is the approximate aim point for the surface?
		NV.z = _opt_height;
		NV.x = _d/2. * sin(vaz) + _rec_offset_x;		//[m] x-location of surface at angle vaz, given radius _d/2
		NV.y = _d/2. * cos(vaz) + _rec_offset_y;		//[m] y-location "" "" ""

		//calculate the normal vector
		NV.i = sin(vaz)*cos(_rec_elevation);
		NV.j = cos(vaz)*cos(_rec_elevation);
		NV.k = sin(_rec_elevation);
		break;
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
		//All other types should be simply equal to the user-specified az/zen
		//The approximate aim point is:
		NV.x = _rec_offset_x;
		NV.y = _rec_offset_y;
		NV.z = _opt_height;
		//Calculate the unit vector
		NV.i = sin(_rec_az)*cos(_rec_elevation);
		NV.j = cos(_rec_az)*cos(_rec_elevation);
		NV.k = sin(_rec_elevation);
		break;
	default:
		throw spexception("Unsupported receiver type");
	}

	return;

}

//------------------Scripts------------------

//Initialization call to create the receiver surfaces
void Receiver::DefineReceiverGeometry(int nflux_x, int nflux_y) {

	/* 
	The process of defining receiver geometry for each receiver should be:

	1) Indicate which specific geometry type should be used with "_rec_geom"
	2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
	3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).
	4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
	5) Define the precision of the flux map.
	6) Define the maximum flux for each panel.
	7) Call the method to set up the flux hit test grid.

	Geometries are:
	0	|	Continuous closed cylinder - external
	1	|	Continuous open cylinder - external	
	2	|	Continuous open cylinder - internal cavity
	3	|	Planar rectangle
	4	|	Planar ellipse
	5	|	Discrete closed N-polygon - external	
	6	|	Discrete open N-polygon - external
	7	|	Discrete open N-polygon - internal cavity
	*/

	if(_rec_type == REC_TYPE::CYLINDRICAL){ //External
		//if(! _is_polygon){
		/*continuous external cylinders. Setup shares some common features..*/
			
		//this uses a single curved surface
		_surfaces.resize(1);
				
		FluxSurface *S = &_surfaces.at(0);
		S->setParent(this);

		//Do setup
		Point loc;
		getReceiverOffset( loc );
		S->setSurfaceGeometry( _h, 0., _d/2. );
		S->setSurfaceOffset( loc );
		//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
		Vect nv;
		nv.i = sin(_rec_az)*cos(_rec_elevation);
		nv.j = cos(_rec_az)*cos(_rec_elevation);
		nv.k = sin(_rec_elevation);
		S->setNormalVector(nv); 
						
		if(! _is_open_geom){
			_rec_geom = _is_polygon ? 
				Receiver::REC_GEOM_TYPE::POLYGON_CLOSED : 
				Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED;		/*	0 | Continuous closed cylinder - external	*/

			S->setSurfaceSpanAngle(-Pi,Pi);	//Full surround
            _span_min = -Pi;
            _span_max = Pi; //enforce closedness - overwrite any other values
		}
		else{
			_rec_geom = _is_polygon ? 
				Receiver::REC_GEOM_TYPE::POLYGON_OPEN : 
				Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN;		/*	1 | Continuous open cylinder - external	*/
			//A curved surface that doesn't form a closed circle. Extents are defined by the span angles.
			S->setSurfaceSpanAngle(_span_min, _span_max);
		}
			
		//Default setup will be for a single flux test point on the surface. In more detailed
		//flux mapping runs, this can be changed to whatever the desired resolution is.
		S->setFluxPrecision(nflux_x,nflux_y);
		S->setMaxFlux(_peak_flux);
		S->DefineFluxPoints(_rec_geom);

		//}
		//else{
		//	/* Discrete external cylinders of polygonal shape */

		//	//The flux surface of the polygon will still be represented as a single surface
		//	_surfaces.resize(1);

		//	FluxSurface *S = &_surfaces.front();
		//	S->setParent(this);

		//	//Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset
		//	double pdaz, wpanel;
		//	if(! _is_open_geom){
		//		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_CLOSED;		/*	5 | Discrete closed N-polygon - external	*/

		//		//Calculate the panel width
		//		pdaz = 2.*Pi/double(_n_panels);	//The azimuthal span of each panel
		//		wpanel = _d/2.*tan(pdaz); //Width of each panel
		//							
		//	}
		//	else{
		//		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_OPEN;		/*	6 | Discrete open N-polygon - external	*/

		//		//Calculate the panel width based on the total span angle. The span angle is defined
		//		//such that the minimum bound of the angle passes through (1) a vector from the center of
		//		//the polygon inscribed circle through the centroid of the farthest panel in the CCW 
		//		//direction, and (2) a vector from the center of teh polygon inscribed circle through 
		//		//the centroid of the farthest panel in the CW direction.
		//		pdaz = (_span_max - _span_min)/double(_n_panels-1);
		//		wpanel = _d/2.*tan(pdaz);	//width of each panel
		//			
		//	}
		//	S->setSurfaceGeometry(_h, wpanel);

		//	//Calculate the azimuth angle of the receiver panel
		//	double paz = _panel_rotation + pdaz*double(i);
		//	//Calculate the elevation angle of the panel
		//	double pzen = _rec_elevation*cos(_panel_rotation-paz);
		//	//Set the surface normal vector
		//	Vect nv;
		//	nv.i = sin(paz)*sin(pzen);
		//	nv.j = cos(paz)*sin(pzen);
		//	nv.k = cos(pzen);
		//	S->setNormalVector(nv);

		//	//Calculate the centroid of the panel in global XYZ coords
		//	Point pc;
		//	pc.x = nv.i * _d/2.;
		//	pc.y = nv.j * _d/2.;
		//	pc.z = nv.k * _d/2.;
		//	S->setSurfaceOffset(pc);

		//	//Define the precision of the flux map.
		//	S->setFluxPrecision(nflux_x,nflux_y);
		//	S->setMaxFlux(_peak_flux);
		//	//Call the method to set up the flux hit test grid.
		//	S->DefineFluxPoints(_rec_geom);

		//}
	}
	else if(_rec_type == REC_TYPE::CAVITY){ //Cavity
		if(! _is_polygon){		/*	2 | Continuous open cylinder - internal cavity	*/
			
			//1) Indicate which specific geometry type should be used with "_rec_geom"
			_rec_geom = Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV;

			//2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
			_surfaces.resize(1);
			FluxSurface *S = &_surfaces.at(0);
			S->setParent(this);

			//3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).

			Point loc;
			getReceiverOffset( loc );
			S->setSurfaceGeometry( _h, _w, _d/2. );
			S->setSurfaceOffset( loc );
			//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
			Vect nv;
			nv.i = sin(_rec_az)*cos(_rec_elevation);
			nv.j = cos(_rec_az)*cos(_rec_elevation);
			nv.k = sin(_rec_elevation);
			S->setNormalVector(nv); 
						
			//4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
			S->setSurfaceSpanAngle(_span_min, _span_max);
						
			//5) Define the precision of the flux map.
			
			//Default setup will be for a single flux test point on the surface. In more detailed
			//flux mapping runs, this can be changed to whatever the desired resolution is.
			S->setFluxPrecision(nflux_x,nflux_y);
			
			//6) Define the maximum flux for each panel.
			S->setMaxFlux(_peak_flux);
			
			//7) Call the method to set up the flux hit test grid.
			S->DefineFluxPoints(_rec_geom);			

		}
		else{
			_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_CAV;			/*	7 | Discrete open N-polygon - internal cavity	*/

			//Use the number of panels as the number of polygon facets. Each facet is its own surface.
			_surfaces.resize(_n_panels);

			for(int i=0; i<_n_panels; i++){
				FluxSurface *S = &_surfaces.at(i);
				S->setParent(this);

				//Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset
				double pdaz, wpanel;
				
				/*
				Calculate the panel width based on the total span angle. The span angle is defined
				such that the minimum bound of the angle passes through (1) a vector from the center of
				the polygon inscribed circle through the centroid of the farthest panel in the CCW 
				direction, and (2) a vector from the center of teh polygon inscribed circle through 
				the centroid of the farthest panel in the CW direction.
				*/
				pdaz = (_span_max - _span_min)/double(_n_panels-1);
				wpanel = _d/2.*tan(pdaz);	//width of each panel
					
				
				S->setSurfaceGeometry(_h, wpanel);

				//Calculate the azimuth angle of the receiver panel
				double paz = _panel_rotation + pdaz*double(i);
				//Calculate the elevation angle of the panel
				double pzen = _rec_elevation*cos(_panel_rotation-paz);
				//Set the surface normal vector
				Vect nv;
				nv.i = -sin(paz)*sin(pzen);
				nv.j = -cos(paz)*sin(pzen);
				nv.k = -cos(pzen);
				S->setNormalVector(nv);

				//Calculate the centroid of the panel in global XYZ coords
				Point pc;
				pc.x = nv.i * _d/2.;
				pc.y = nv.j * _d/2.;
				pc.z = nv.k * _d/2.;
				S->setSurfaceOffset(pc);

				//Define the precision of the flux map.
				S->setFluxPrecision(nflux_x,nflux_y);
				S->setMaxFlux(_peak_flux);
				//Call the method to set up the flux hit test grid.
				S->DefineFluxPoints(_rec_geom);

			}

		}
	}
	else if(_rec_type == REC_TYPE::FLAT_PLATE){ //Flat plate
		//1) Indicate which specific geometry type should be used with "_rec_geom"
		if(_aperture_type == 0){
			_rec_geom = Receiver::REC_GEOM_TYPE::PLANE_RECT;			/*	3 | Planar rectangle	*/
		}
		else{
			_rec_geom = Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE;			/* 4 | Planar ellipse		*/
		}
			
		//2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
		_surfaces.resize(1);
		FluxSurface *S = &_surfaces.at(0);

		//3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).

		Point loc;
		getReceiverOffset( loc );
		S->setSurfaceGeometry( _h, _w, 0. );
		S->setSurfaceOffset( loc );
		//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
		Vect nv;
		nv.i = sin(_rec_az)*cos(_rec_elevation);
		nv.j = cos(_rec_az)*cos(_rec_elevation);
		nv.k = sin(_rec_elevation);
		S->setNormalVector(nv);	
						
		//4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
		S->setSurfaceSpanAngle(-pi/2., pi/2.);	
						
		//5) Define the precision of the flux map.
		S->setFluxPrecision(nflux_x,nflux_y);
			
		//6) Define the maximum flux for each panel.
		S->setMaxFlux(_peak_flux);
			
		//7) Call the method to set up the flux hit test grid.
		S->DefineFluxPoints(_rec_geom);	

	}

	//Set up the absorber panels


}

void Receiver::CalculateAbsorberArea(){

	/* 
	Calculate the receiver absorber surface area based on the geometry type. This doesn't consider
	the area of individual tubes or elements, only the area of the major geometrical surfaces.

	The local variable _absorber_area is set, which can be accessed via
	getReceiverAbsorberArea()
	*/

	int recgeom = getReceiverGeomType();

	switch (recgeom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
		_absorber_area = _h * _d * pi;
		break;
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
		_absorber_area = _h * _d * fabs(_span_max - _span_min)/2.;
		break;
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
		_absorber_area = _h * _w;
		break;
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
		_absorber_area = pi * _h * _w/4.;
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
		_absorber_area = _h * (double)_n_panels * _d/2.*tan(2.*pi/_n_panels);
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_OPEN:
	case Receiver::REC_GEOM_TYPE::POLYGON_CAV:
		_absorber_area = _h * (double)_n_panels * _d/2.*tan(fabs(_span_max - _span_min)/(double)(_n_panels-1));
		break;
	default:
		break;
	}
	
}

void Receiver::CalculateThermalLoss(double load, double v_wind){
	/* 
	Calculate the thermal loss from the receiver. Update the local values of thermal and piping loss.

    _therm_loss [MWt]   Local value updated
    _piping_loss [MWt]   Local value updated

	Load is a normalized thermal load for the receiver. 
	V_wind is m/s.
	*/

	double
		fload = 0.,
		fwind = 0.;
	for(int i=0; i<(int)_therm_loss_load.ncells(); i++)
		fload += _therm_loss_load.at(i)*pow(load, i);
	for(int i=0; i<(int)_therm_loss_wind.ncells(); i++)
		fwind += _therm_loss_wind.at(i)*pow(v_wind, i);

	_therm_loss = _therm_loss_base * fload * fwind * _absorber_area *1.e-3;    //_therm_loss_base [kWt/m2]

	//piping
	_piping_loss = (_piping_loss_coef * _opt_height + _piping_loss_const)*1.e-3;

}

void Receiver::CalculateThermalEfficiency(double dni, double dni_des, double v_wind, double q_des){
    /* 
    Calculate thermal efficiency and update local values.

    Inputs:
        DNI         W/m2    DNI at current time
        dni_des     W/m2    DNI at system design point
        v_wind      m/s     Wind velocity at current time
        q_des       MWt     Design-point receiver output power

    Sets:
        _thermal_eff
        _therm_loss     (via CalculateThermalLoss)
        _piping_loss    (via CalculateThermalLoss)
    */

	double load = dni/dni_des;
	CalculateThermalLoss(load, v_wind);

    _thermal_eff = 1. - _therm_loss / (_therm_loss + q_des );

}

double Receiver::CalculateApparentDiameter(Point &Hloc){ 
	/* 
	[m] Return the apparent receiver diameter given the polygonal structure

	Take the specified heliostat location, the number of receiver panels, and the orientation
	of the primary receiver panel, and calculate the apparent width of the reciever.
	
	This convention assumes that the receiver diameter CIRCUMSCRIBES all panels. That is, 
	the maximum receiver apparent width is the specified receiver diameter.
	*/

	//only valid for cylindrical receivers
	switch (_rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
		return _d;
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
	{
		//First determine the azimuthal span between the heliostat location vector and the receiver 
		//main panel normal vector
		double alpha = fabs(atan2(Hloc.x, Hloc.y) - _rec_az);
		//Calculate the difference between the angle and the nearest panel normal
		double theta_hat = fmod(alpha, 2.*pi/_n_panels);
		//finally the width is:
		return cos(theta_hat)*_d;
		break;
	}
	default:
		throw spexception("Attempting to calculate an apparent diameter for an unsupported receiver geometry.");
		break;
	}




}
