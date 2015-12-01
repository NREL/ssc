#include "Heliostat.h"
#include "SolarField.h"
#include "Flux.h"
#include "heliodata.h"
#include "math.h"
#include <vector>
using namespace std;

//declare referenced classes

//Accessors
double Heliostat::calcTotalEfficiency(){ return eff_data.calcTotalEfficiency(); }
int Heliostat::getId(){return _id;}
int *Heliostat::getGroupId(){return _group;}		//(row,col) nodes
bool Heliostat::getInLayout(){return _in_layout;}
double Heliostat::getWidth(){return _is_round ? _dm : _wm;} //[m]
double Heliostat::getHeight(){return _is_round ? _dm : _hm;} //[m]
double Heliostat::getCollisionRadius(){return _rcoll;}
double Heliostat::getWGap(){return _wgap;} //[m] Gap in the horizontal direction (width)
double Heliostat::getHGap(){return _hgap;} //[m] Gap in the vertical direction
double Heliostat::getArea(){return _area;}
double Heliostat::getReflectiveAreaDerate(){return _densmr;}
double Heliostat::getFocalX(){return _xfocal;}
double Heliostat::getFocalY(){return _yfocal;}
double Heliostat::getSlantRange(){return _slant;}	//[m]
double Heliostat::getCantRadius(){return _rcant;}	//[m]
int Heliostat::getCantDay(){return _dcant;}
double Heliostat::getCantHour(){return _hcant;}
double Heliostat::getCantVectScale(){return _cant_vect_scale;}
int Heliostat::getNumCantY(){return _ncanty;}
int Heliostat::getNumCantX(){return _ncantx;}
int Heliostat::getCantMethod(){return _cant_method;}
int Heliostat::getFocusMethod(){return _focus_method;}
Receiver *Heliostat::getWhichReceiver(){return _which_rec;}
int Heliostat::getType(){return _type;}
int Heliostat::getTemplateOrder(){return _template_order; }
double Heliostat::getRadialPos(){ return sqrt( pow(_location.x, 2) + pow(_location.y, 2) + pow(_location.z, 2) ); }
double Heliostat::getAzimuthalPos(){return atan2(_location.x, _location.y); }	//Radians
matrix_t<Reflector> *Heliostat::getPanels(){return &_panels;}
Vect *Heliostat::getTrackVector(){return &_track;}	//return the tracking vector
Vect *Heliostat::getTowerVector(){return &_tower_vect;} // return the helio-tower unit vector
Vect *Heliostat::getCantVector(){return &_cant_vect;}	//Return the canting vector (not normalized)
Point *Heliostat::getLocation(){return &_location;} //Get location vector
Point *Heliostat::getAimPoint(){return &_aim_point;}	//Get the heliostat aim point on the receiver
Point *Heliostat::getAimPointFluxPlane(){return &_aim_fluxplane;}	//aim point in the flux plane coordinates 
helio_perf_data *Heliostat::getEfficiencyObject(){return &eff_data;}
double Heliostat::getTotalReflectivity(){return eff_data.reflectivity * eff_data.soiling;}
double Heliostat::getEfficiencyTotal(){return eff_data.eta_tot;}
double Heliostat::getEfficiencyCosine(){return eff_data.eta_cos;}
double Heliostat::getEfficiencyAtten(){return eff_data.eta_att;}
double Heliostat::getEfficiencyIntercept(){return eff_data.eta_int;}
double Heliostat::getEfficiencyBlock(){return eff_data.eta_block;}
double Heliostat::getEfficiencyShading(){return eff_data.eta_shadow;}
double Heliostat::getEfficiencyCloudiness(){return eff_data.eta_cloud;}
double Heliostat::getPowerToReceiver(){return eff_data.power_to_rec;}
double Heliostat::getPowerValue(){return eff_data.power_value;}
double Heliostat::getRankingMetricValue(){return eff_data.rank_metric;}
double Heliostat::getAzimuthTrack(){return _azimuth;}
double Heliostat::getZenithTrack(){return _zenith;}
vector<Heliostat*> *Heliostat::getNeighborList(){return _neighbors;}
vector<Point> *Heliostat::getCornerCoords(){return &_corners;}
vector<Point> *Heliostat::getShadowCoords(){return &_shadow;}
matrix_t<double> *Heliostat::getMirrorShapeNormCoefObject(){return &_mu_MN;}
matrix_t<double> *Heliostat::getMirrorShapeCoefObject(){return &_mu_M;}
matrix_t<double> *Heliostat::getSunShapeCoefObject(){return &_mu_S;}
matrix_t<double> *Heliostat::getErrorDistCoefObject(){return &_mu_G;}
matrix_t<double> *Heliostat::getFluxMomentsObject(){return &_mu_F;}
matrix_t<double> *Heliostat::getHermiteCoefObject(){return &_hcoef;}
matrix_t<double> *Heliostat::getHermiteNormCoefObject(){return &_hc_tht;}
double *Heliostat::getImageSize(){return _image_size_xy;}
void Heliostat::getImageSize(double &sigx_n, double &sigy_n){ sigx_n = _image_size_xy[0]; sigy_n = _image_size_xy[1];}
string *Heliostat::getHeliostatName(){return &_helio_name;}
Heliostat* Heliostat::getMasterTemplate(){return _master_template;}

bool Heliostat::IsRound(){return _is_round;}	//Fetch
void Heliostat::IsRound(bool setting){_is_round = setting;}	//Set
bool Heliostat::IsUserCant(){return _is_user_canted;} //Fetch
void Heliostat::IsUserCant(bool setting){_is_user_canted = setting;} //Set
bool Heliostat::IsUserFocus(){return _focus_method == 3 ? true : false; }
void Heliostat::IsUserFocus(bool setting){if(setting) _focus_method = 3;}
bool Heliostat::IsFacetDetail(){return _is_faceted;}	//Is characterization of multiple facets required?
void Heliostat::IsFacetDetail(bool setting){_is_faceted = setting;}
bool Heliostat::IsEnabled(){return _is_enabled;}

void Heliostat::setId(int id){_id = id;}
void Heliostat::setGroupId(int row, int col){_group[0] = row; _group[1] = col;}
void Heliostat::setType(int type){_type = type;}
void Heliostat::setInLayout(bool in_layout){_in_layout = in_layout;}
void Heliostat::setNeighborList(vector<Heliostat*> *list){_neighbors = list;}
void Heliostat::setWidth(double &val, bool update_cant){_wm = val; if(update_cant){installPanels();}}
void Heliostat::setHeight(double &val, bool update_cant){_hm = val; if(update_cant){installPanels();}}
void Heliostat::setDiameter(double &val, bool update_cant){_dm = val; if(update_cant){installPanels();}}
void Heliostat::setEfficiencyCosine(double eta_cos){eff_data.eta_cos = fmin(fmax(eta_cos,0.),1.);}
void Heliostat::setEfficiencyAtmAtten(double eta_att){eff_data.eta_att = eta_att;}
void Heliostat::setEfficiencyIntercept(double eta_int){eff_data.eta_int = eta_int;}
void Heliostat::setEfficiencyBlocking(double eta_block){eff_data.eta_block = eta_block;}
void Heliostat::setEfficiencyShading(double eta_shadow){eff_data.eta_shadow = eta_shadow;}
void Heliostat::setEfficiencyCloudiness(double eta_cloud){eff_data.eta_cloud = eta_cloud;}
void Heliostat::setEfficiencyTotal(double eta_tot){eff_data.eta_tot = eta_tot;}
void Heliostat::setRankingMetricValue(double rval){eff_data.rank_metric = rval;}
void Heliostat::setAimPointFluxPlane(Point &Aim){_aim_fluxplane.Set( Aim.x, Aim.y, Aim.z );}
void Heliostat::setAimPointFluxPlane(double x, double y, double z){ _aim_fluxplane.Set(x, y, z); }
void Heliostat::setTrackVector(Vect &tr){ _track = tr; }	//Set the tracking vector
void Heliostat::setTowerVector(Vect &tow){ _tower_vect = tow; } //Set the helio-tower vector
void Heliostat::setTrackAngleZenith(double zenith){_zenith = zenith;}
void Heliostat::setTrackAngleAzimuth(double azimuth){_azimuth = azimuth;}
void Heliostat::setTrackAngles(double azimuth, double zenith){_zenith = zenith; _azimuth = azimuth;}
void Heliostat::setTemplateOrder(int tord){_template_order = tord; }
void Heliostat::setCantMethod(int method){
	if(method != CANT_TYPE::USER_VECTOR )
        _cant_method = method;
	if(method == CANT_TYPE::ON_AXIS_USER )
        _is_user_canted = true; 
    else 
        _is_user_canted = false;
};
void Heliostat::setCantVector(Vect &cant){_cant_vect.Set(cant.i, cant.j, cant.k);}
void Heliostat::setCantVector(double cant[3]){_cant_vect.Set(cant[0], cant[1], cant[2]);}
void Heliostat::setFocusMethod(int method){_focus_method = method;}
void Heliostat::setSlantRange(double L){_slant = L; if(_cant_method == CANT_TYPE::AT_SLANT){_xfocal = L; _yfocal = L;}}
void Heliostat::setFocalLengthX(double L){_xfocal = L;}
void Heliostat::setFocalLengthY(double L){_yfocal = L;}
void Heliostat::setFocalLength(double L){_yfocal = L; _xfocal = L;}
void Heliostat::setCantRadius(double L){_rcant = L;}
void Heliostat::setErrorAngular(matrix_t<double> &E){setErrorAngular(E.at(0), E.at(1));}
void Heliostat::setErrorAngular(double sigaz, double sigel){_sigaz = sigaz; _sigty = sigel;}
void Heliostat::setErrorSurface(matrix_t<double> &E){setErrorSurface(E.at(0), E.at(1));}
void Heliostat::setErrorSurface(double sigsx, double sigsy){_sigsx = sigsx; _sigsy = sigsy;}
void Heliostat::setErrorReflected(matrix_t<double> &E){setErrorReflected(E.at(0), E.at(1));}
void Heliostat::setErrorReflected(double sigtx, double sigty){_sigtx = sigtx; _sigty = sigty;}
//void setWhichReceiver(int rec){_which_rec = rec;}
void Heliostat::setWhichReceiver(Receiver *rec){_which_rec = rec;}
void Heliostat::setPowerToReceiver(double P){eff_data.power_to_rec = P;}
void Heliostat::setPowerValue(double P){eff_data.power_value = P;}
void Heliostat::setImageSize(double sigx_n, double sigy_n){ _image_size_xy[0] = sigx_n; _image_size_xy[1] = sigy_n;}
void Heliostat::setMasterTemplate(Heliostat *htemp){_master_template = htemp;}

//Set the default values for the heliostat class
void Heliostat::setDefaults () {
	
	var_map V;	//empty variable map, all will be set to the default value
	Create(V);
		
}

void Heliostat::resetMetrics(){
	eff_data.resetMetrics();
}

void Heliostat::Create(var_map &V)
{
	setVar("helio_name", _helio_name, V, "Template 1");		//Heliostat template name
	setVar("id", _id, V, -1, "[-1,9e99]");		//Unique ID number for the heliostat template
	setVar("is_enabled", _is_enabled, V, true);		//Is template enabled?
	setVar("temp_rad_min", _temp_rad_min, V, 0.75, "[0,9e9]");		//Minimum radius at which this heliostat geometry can be used
	setVar("temp_rad_max", _temp_rad_max, V, 7.5, "[0,9e9]");		//Maximum radius at which this heliostat geometry can be used
	setVar("temp_az_min", _temp_az_min, V, 0., "[-180.,180]");		//Angular boundary for heliostat geometry - on the counter-clockwise side of the region
	setVar("temp_az_max", _temp_az_max, V, 360., "[-180.,180]");		//Angular boundary for heliostat geometry - on the clockwise side of the region
	setVar("location_x", _location.x, V, 0., "[-9e9,9e9]");		//X Location of heliostat in the field (0,0,Zt) is the location of the tower base
	setVar("location_y", _location.y, V, 0., "[-9e9,9e9]");		//Y Location of heliostat in the field (0,0,Zt) is the location of the tower base
	setVar("location_z", _location.z, V, 0., "[-9e9,9e9]");		//Z Location of heliostat in the field (0,0,Zt) is the location of the tower base
	setVar("is_round", _is_round, V, 0 );		//Is the heliostat round (true) or rectangular (false)
	setVar("focus_method", _focus_method, V, 1, "[0,3]");		//The focusing method {0=Flat, 1=Each at slant, 2=Average of group, 3=User defined}
	setVar("is_xfocus", _is_xfocus, V, true );		//Reflector is focused in with respect to the heliostat X axis
	setVar("is_yfocus", _is_yfocus, V, true );		//Reflector is focused in with respect to the heliostat Y axis
	setVar("is_focal_equal", _is_focal_equal, V, true);		//Both the X and Y focal lengths will use a single value as indicated by the X focal length
	setVar("x_focal_length", _xfocal, V, 1000., "(0.,9e99]");		//Reflector focal length with respect to the heliostat X (horizontal) axis
	setVar("y_focal_length", _yfocal, V, 1000., "(0.,9e99]");		//Reflector focal length with respect to the heliostat Y (vertical) axis
	setVar("is_faceted", _is_faceted, V, true);		//The number of reflective panels per heliostat is greater than 1
	setVar("cant_method", _cant_method, V, -1, "[-1,3]");		//Integer to specify the canting method {0=none, -1=Cant on-axis equal to slant range, 1=user-defined on-axis, 3=user-defined off-axis at hour + day}
	setVar("type", _type, V, 0, "[0,9e9]");		//Integer used to group heliostats into geometries within a field, (e.g. 5 different focal length designs)
	setVar("n_cant_x", _ncantx, V, 2, "[1,1000]");		//Number of cant panels in the X direction
	setVar("n_cant_y", _ncanty, V, 8, "[1,1000]");		//Number of cant panels in the Y direction
	setVar("n_rot_axes", _nrotax, V, 2, "[0,2]");		//Number of rotational axes on the tracker (1 for LFresnel, 2 for heliostat)
	setVar("width", _wm, V, 12.2, "(0.,1000.]");		//Width of the heliostat structure
	setVar("height", _hm, V, 12.2, "(0.,1000.]");		//Height of the heliostat structure
	setVar("diameter", _dm, V, 13.77, "(0.,1000.]");		//Diameter of the heliostat structure (round heliostats only)
	setVar("x_gap", _wgap, V, 0., "[0.,1000.]");		//Separation between panels in the horizontal direction
	setVar("y_gap", _hgap, V, 0., "[0.,1000.]");		//Separation between panels in the vertical direction
	setVar("cant_hour", _hcant, V, 0., "(-12.,12.]");		//Hours past noon at which the mirror panels are canted (-12 to 12)
	setVar("cant_day", _dcant, V, 82, "[1,365]");		//Day of the year used for canting the heliostats (1-365)
	setVar("cant_radius", _rcant, V, 1000., "(0.,9e99]");		//Radius for canting focal point assuming on-axis canting
	setVar("is_cant_rad_scaled", _is_cant_rad_scaled, V, true);		//The cant radius scales with tower height
	setVar("cant_rad_scaled", _rcant_scaled, V, 1., "[-9e9,9e9]");		//Canting radius value (absolute value if radius is not scaled, multiplied by tower height if scaled)
	setVar("cant_vect_i", _cant_vect.i, V, 0., "[-9e9,9e9]");		//Canting vector - x-component
	setVar("cant_vect_j", _cant_vect.j, V, 0., "[-9e9,9e9]");		//Canting vector y-component
	setVar("cant_vect_k", _cant_vect.k, V, 1., "[-9e9,9e9]");		//Canting vector z-component
	setVar("cant_vect_scale", _cant_vect_scale, V, 400., "[-9e9,9e9]");		//Value to scale the canting unit vector to determine actual canting magnitude
	setVar("is_cant_vect_slant", _is_cant_vect_slant, V, false);		//Multiply the canting vector by the slant range
	setVar("reflect_ratio", _densmr, V, 0.97, "(0.,1.]");		//Ratio of mirror area to total area of the heliostat defined by wm x hm
	setVar("reflectivity", eff_data.reflectivity, V, 0.95, "(0.,1.]");		//Average reflectivity (clean) of the mirrored surface
	setVar("soiling", eff_data.soiling, V, 0.95, "[0.,1.]");		//Average soiling factor
	setVar("err_elevation", _sigel, V, 0.00075, "[0.,1000.]");		//Standard deviation of the normal error dist. of the elevation angle
	setVar("err_azimuth", _sigaz, V, 0.00075, "[0.,1000.]");		//Standard deviation of the normal error dist. of the azimuth angle
	setVar("err_surface_x", _sigsx, V, 0.001, "[0.,1000.]");		//Std.dev. of the normal error dist. of the reflective surface normal in the X (horizontal)
	setVar("err_surface_y", _sigsy, V, 0.001, "[0.,1000.]");		//Same as above, but in the vertical direction
	setVar("err_reflect_x", _sigtx, V, 0., "[0.,1000.]");		//error in reflected vector (horiz.) caused by atmospheric refraction, tower sway, etc.
	setVar("err_reflect_y", _sigty, V, 0., "[0.,1000.]");		//error in reflected vector (vert.) caused by atmospheric refraction, tower sway, etc.
	setVar("rvel_max_x", _maxrvelx, V, 1000., "(0.,9e99]");		//maximum rotational velocity about the x axis
	setVar("rvel_max_y", _maxrvelz, V, 1000., "(0.,9e99]");		//maximum rotational velocity about the z axis
	setVar("r_collision", _rcoll, V, 0., "(0.,1000.]");		//Heliostat reflector maximum footprint diameter
    setVar("track_method", _track_method, V, 0);		//Specify how often heliostats update their tracking position 
	setVar("track_period", _track_period, V, 10., "[0,99999]");		//The amount of time between tracking updates for each heliostat

	_track = Vect(); //The tracking vector for the heliostat, defaults to 0,0,1
	_tower_vect = Vect();  //Heliostat-to-tower unit vector
	
	//units
	_temp_az_min *= d2r;		//Angular boundary for heliostat geometry - on the counter-clockwise side of the region
	_temp_az_max *= d2r;		//Angular boundary for heliostat geometry - on the clockwise side of the region
	
    
	//Now define panel geometry
	installPanels();

}

void Heliostat::getErrorAngular(double err[2]){
	err[0] = _sigaz; 
	err[1] = _sigel; 
}

void Heliostat::getErrorSurface(double err[2]){
	err[0] = _sigsx; 
	err[1] = _sigsy; 
}

void Heliostat::getErrorReflected(double err[2]){
	err[0] = _sigtx; 
	err[1] = _sigty; 
}

void Heliostat::getSummaryResults( vector<double> &results){
	/* 
	Fill the vector "results" with performance metrics of interest
	*/
	results.resize( eff_data.n_metric );
	for(int i=0; i<eff_data.n_metric; i++) results.at(i) = eff_data.getDataByIndex(i); 
	return;
}

void Heliostat::setAimPoint(double x, double y, double z){
	//Set the heliostat aim point
	_aim_point.x = x;
	_aim_point.y = y;
	_aim_point.z = z;
}

void Heliostat::setAimPoint(Point &Aim){
	setAimPoint(Aim.x, Aim.y, Aim.z);
}

void Heliostat::installPanels() {
	/*
	This method uses the inputs to define the location and pointing vector of each
	panel on the heliostat. 

	DELSOL3 lines 6494-6520

	Note that in DELSOL3, this originally is part of the flux algorithm. The panel
	arrangement is more conveniently conceptualized as an attribute of the heliostat
	rather than as part of the flux algorithm, so it is placed here instead.
	*/

    //Calculate the collision radius
	if(_is_round){
		_rcoll = _dm/2.;
		_area = Pi*pow(_dm/2.,2)*_densmr;
	}
	else{
		_rcoll = sqrt( pow(_hm/2.,2) + pow(_wm/2.,2) );
		_area = _wm * _hm * _densmr             //width * height * structural density is the base area
                - _wgap * _hm * (_ncantx - 1) - _hgap * _wm * (_ncanty - 1)     //subtract off gap areas
                + (_ncanty - 1)*(_ncantx - 1)*_hgap * _wgap;        //but don't double-count the little squares in both gaps
	}

	//Initialize the image plane image size for this heliostat to zero until it's calculated in the Flux methods
	setImageSize(0.,0.);

	if(_is_round){

		/* 
		This configuration allows only 1 facet per heliostat. By default, the canting is normal.
		*/

		//Correct the settings if any are inconsistent
		_ncantx = 1;
		_ncanty = 1;
		_panels.resize(1,1);

		_panels.at(0,0).setId(0);
		_panels.at(0,0).setType(2);	//Circular
		_panels.at(0,0).setDiameter(_dm);
		_panels.at(0,0).setHeight(_dm);
		_panels.at(0,0).setWidth(_dm);
		_panels.at(0,0).setPosition(0.,0.,0.);
		_panels.at(0,0).setAim(0.,0.,1.);

	}
	else{	//Rectangular heliostats

		//Variable declarations
		double dx, dy, x, y;
	
		//--------Calculate canting from other inputs-----------
		//Calculate height and width of each facet
		dx = (_wm - _wgap * (_ncantx - 1.) ) / (double)_ncantx;	//[m] width of each canting panel
		dy = (_hm - _hgap * (_ncanty - 1.) ) / (double)_ncanty;	//[m] height of each panel

		int id=0;
		_panels.resize(_ncanty, _ncantx);
        
		//back-calculate the aim point
        Point paim;     //heliostat aimpoint
		paim.x = _location.x + _slant*_tower_vect.i;
		paim.y = _location.y + _slant*_tower_vect.j;
		paim.z = _location.z + _slant*_tower_vect.k;

        //initialize X and Y location of the facet
		y = -_hm/2. + dy*0.5;

		for (int j=0; j<_ncanty; j++) {
            //initialize the X location of the facet
		    x = -_wm/2. + dx*0.5;

			for (int i=0; i<_ncantx; i++) {
				//Assign an ID
				_panels.at(j,i).setId(id); id++;
				_panels.at(j,i).setType(1);	//Type=1, rectangular panel
				_panels.at(j,i).setWidth(dx);
				_panels.at(j,i).setHeight(dy);
				//Set the position in the reflector plane. Assume the centroid is in the plane (z=0)
				_panels.at(j,i).setPosition(x, y, 0.0);
				//Determine how each panel is canted
				switch(_cant_method)
				{
                case CANT_TYPE::AT_SLANT:	//Individual on-axis cant at distance equal to the slant range
                {
					double hyp = sqrt( pow(_slant,2) + pow(x, 2) + pow(y, 2) );	//hypotenuse length
					_panels.at(j,i).setAim(-x/hyp, -y/hyp, 2.*_slant/hyp);
					break;
                }
                case CANT_TYPE::FLAT:		//no canting
					_panels.at(j,i).setAim(0.,0.,1.);
					break;
                case CANT_TYPE::ON_AXIS_USER:		//User-defined on-axis canting. Canting specified in array.
                {
					double hyp = sqrt( pow(_rcant,2) + pow(x, 2) + pow(y, 2) );	//cant focal length
					_panels.at(j,i).setAim(-x/hyp, -y/hyp, 2.*_rcant/hyp);
					break;
                }
                case CANT_TYPE::AT_DAY_HOUR:		//Individual off-axis cant at time defined by tracking vector
                {
					//Calculate the tracking azimuth/zenith based on the tracking vector
					double track_az = atan2(_track.i,_track.j);
					double track_zen = acos(_track.k);
				
					//Calculate the panel's actual x-y-z location w/r/t the global coordinates
					double prad = sqrt(pow(x,2)+pow(y*sin(track_zen),2));	//the radius of the panel from the heliostat centroid
					double theta_rot = atan2(x,y);	//angle of rotation of the centroid of the point w/r/t the heliostat coordinates
                    Point pg;
					pg.x = _location.x + prad*sin(track_az+theta_rot);
					pg.y = _location.y + prad*cos(track_az+theta_rot);
					pg.z = _location.z + y*sin(track_zen);

					//determine the vector from the panel centroid to the aim point
					double pslant = sqrt( pow(pg.x - paim.x, 2) + pow(pg.y - paim.y, 2) + pow(pg.z - paim.z, 2));
					Vect pref;
                    pref.i = (paim.x - pg.x)/pslant;
					pref.j = (paim.y - pg.y)/pslant;
					pref.k = (paim.z - pg.z)/pslant;

					//back calculate the sun vector from the heliostat normal and receiver vectors
					Vect s_hat;
                    s_hat.i = 2.*_track.i - pref.i;
					s_hat.j = 2.*_track.j - pref.j;
					s_hat.k = 2.*_track.k - pref.k;

					//The canting correction vector is the average of the sun vector and the reflection vector subtracting 
					//the total heliostat tracking vector
					_panels.at(j,i).setAim( (s_hat.i + pref.i)/2. - _track.i, (s_hat.j + pref.j)/2. - _track.j, (s_hat.k + pref.k)/2. - _track.k + 1.);
				
					break;
                }
                case CANT_TYPE::USER_VECTOR:
                {
                    //throw spexception("The user cant vector option is not correctly implemented in the installPanels() algorithm. Contact support for help resolving this issue.");

                    //The canting correction vector ensures that the user-specified vector is the focus of the canting operation. 
                    Vect paim;
                    double rscale = _rcant_scaled ? _cant_vect_scale : 1.;

                    /* 
                    coordinate system: looking at the heliostat face on, +x is horizontal, +y vertical. Vector is assumed to come out of the 
                    plane containing the heliostat in the direction of the viewer (i.e. it hits you in the face). This is -z. The if the 
                    vector tilts to the right as viewed, this is +i / +x, up is +j / +y.
                    */
                    paim.Set( _cant_vect.i * rscale - x, _cant_vect.j * rscale - y, _cant_vect.k * rscale );
                    //normalize
                    Toolbox::unitvect( paim );
                    _panels.at(j,i).setAim( paim );

                    break;
                }
				default:
                    throw spexception("The requested canting option is not correctly implemented in the installPanels() algorithm. Contact support for help resolving this issue.");
				}

                //increment the x panel position
                x += dx + _wgap;
			}
            //increment the y panel position
            y += dy + _hgap;
		}
	}
	

}

void Heliostat::updateTrackVector(Vect &sunvect) {
	/*
	Calculates the tracking vector given a solar position in "Ambient"
	and a receiver in "Receiver".

	Updates the coordinates of the heliostat corners for shadowing/blocking calculations.

	Do not update the aim point. This method uses the currently assigned aim point.

	This also updates:
	_track			| setTrackVector()		| The tracking vector for the heliostat
	_azimuth		| setTrackAngles()		| The tracking azimuth angle
	_zenith			| setTrackAngles()		| The tracking zenith angle
	_corners		| none					| The location of the heliostat corners in global coordinates (for shadowing and blocking)

	Store the new tracking vector in _track

	From Snell's law, n_hat = (s_hat + t_hat) / mag(s_hat + t_hat)
		where:
		n_hat is the normal tracking vector
		s_hat is the heliostat to sun vector
		t_hat is the helostat to receiver vector
	*/

	Vect 
		n_hat, //tracking vector
		s_hat, //heliostat to sun
		t_hat; //heliostat to receiver
		
	//Get the solar position vector from the ambient class
	s_hat = sunvect;

	//Create a vector between the heliostat and the aim point
	t_hat.Set(_aim_point.x - _location.x, _aim_point.y - _location.y, _aim_point.z - _location.z);
	Toolbox::unitvect(t_hat);
			
	//Use the approximate tower vector t_hat to determine the tracking vector
	Vect ts;
		
	ts.i = t_hat.i + s_hat.i;
	ts.j = t_hat.j + s_hat.j;
	ts.k = t_hat.k + s_hat.k; //break down to save on calculation
	double ts_mag = sqrt( pow(ts.i, 2) + pow(ts.j, 2) + pow(ts.k, 2) );
	n_hat.i = ts.i/ts_mag;
	n_hat.j = ts.j/ts_mag;
	n_hat.k = ts.k/ts_mag;
		
	//Set the Heliostat object tracking vector
	setTrackVector(n_hat);
	//Set the heliostat to tower vector
	setTowerVector(t_hat);
	//Set the tracking angles
	setTrackAngles(atan2(n_hat.i,n_hat.j), acos(n_hat.k));
	
	/*Calculate the location in global coordinates of the top two heliostat corners. Note that 
	by the azimuth convention where North is 0deg, the upper edges of the heliostat will begin on
	the southernmost edge of the heliostat.
		
	Assume that the heliostat is starting out facing upward in the z direction with the 
	upper and lower edges parallel to the global x axis (i.e. zenth=0, azimuth=0)
	*/
	if(! _is_round){
		_corners.resize(4);
	
		_corners.at(0).Set(-_wm/2., -_hm/2., 0.);	//Upper right corner
		_corners.at(1).Set(_wm/2., -_hm/2., 0.);	//upper left corner
		_corners.at(2).Set(_wm/2., _hm/2., 0.);	//lower left
		_corners.at(3).Set(-_wm/2., _hm/2., 0.);	//lower right
		
		for(int i=0; i<4; i++){ //For each point of interest...
			//Rotate first about the x axis (zenith)
			Toolbox::rotation(_zenith, 0, _corners.at(i)); 
			//Now rotate about the z-axis (azimuth)
			Toolbox::rotation(_azimuth, 2, _corners.at(i));
			//Move from heliostat coordinates to global coordinates
			_corners.at(i).Add(_location.x, _location.y, _location.z); 
		}
	}
	else{ 
		//no corner geometry to consider for round heliostats
	}
	
	return;

}

void Heliostat::calcAndSetAimPointFluxPlane(Point &aimpos_abs, Receiver &Rec, Heliostat &H)
{
    /* 
    Given a particular aim point in space, translate the position to an aimpoint on the actual
    flux plane of the receiver. The original aimpoint may not necessarily be on the plane of the 
    receiver, but the final aim point will be.
    */    
    
    Point aimpos(aimpos_abs);    
    
    //rotate into flux plane coordinates
	Toolbox::rotation(H.pi - Rec.getReceiverAzimuth(),2,aimpos);
	Toolbox::rotation(H.pi/2. - Rec.getReceiverElevation(),0,aimpos);
	//The z component should be very small, so zero out
	if( fabs(aimpos.z) < 1.e-6 ) aimpos.z = 0.;
	//The X and Y coordinates now indicate the image plane position
	H.setAimPointFluxPlane(aimpos.x, aimpos.y, aimpos.z);
}

void Heliostat::setLocation(double x, double y, double z)
{
	//Set the location 
	_location.x = x;
	_location.y = y;
	_location.z = z;
}

//--------------Reflector class methods ----------------------

Reflector::Reflector(){
	setDefaults();
}

//Get-Set methods
int Reflector::getId(){return _id;}
double Reflector::getWidth(){return _width;}
double Reflector::getHeight(){return _height;}
double Reflector::getDiameter(){return _diameter;}
double Reflector::getFocalLength(){return _focal_length;}
int Reflector::getType(){return _type;}
PointVect *Reflector::getOrientation(){return &_locate_vector;}
	

void Reflector::setId(int id){_id = id;}
void Reflector::setType(int type){_type = type;}
void Reflector::setWidth(double width){_width = width;}
void Reflector::setHeight(double height){_height = height;}
void Reflector::setDiameter(double diam) {_diameter = diam;}

//Set the default values for the reflector class
void Reflector::setDefaults() {
	//Set default values
	_width = 0.;
	_height = 0.;
	_diameter = 0.;
	_focal_length = 0.;
	_id = -1;
	_type = 1;
	setOrientation(0., 0., 0., 0., 0., 0.);
}

void Reflector::setPosition(double x, double y, double z) {
	_locate_vector.x = x;
	_locate_vector.y = y;
	_locate_vector.z = z;
}

void Reflector::setAim(double i, double j, double k){
	_locate_vector.i = i;
	_locate_vector.j = j;
	_locate_vector.k = k;
}

void Reflector::setAim( Vect &V ){
	_locate_vector.i = V.i;
	_locate_vector.j = V.j;
	_locate_vector.k = V.k;
}

void Reflector::setOrientation(double x, double y, double z, double i, double j, double k){
	setPosition(x,y,z);
	setAim(i,j,k);
}

void Reflector::setOrientation(PointVect &PV){
	setPosition(PV.x, PV.y, PV.z);
	setAim(PV.i, PV.j, PV.k);
}

Reflector *Heliostat::getPanelById(int id){
	for (int j=0; j<_ncantx; j++) {
		for (int i=0; i<_ncanty; i++) {
			if (_panels[j,i].getId() == id){
				return &_panels[j,i];
			}
		}
	}

	//#####call an error here

	return &_panels[0,0];
}

Reflector *Heliostat::getPanel(int row, int col){
	int nr, nc;
	nr = _panels.nrows();
	nc = _panels.ncols();
	if(row < nr && col < nc) {
		return &_panels[row,col];
	}
	else{
		//FLAG -- this should be an error
		throw spexception("Index out of range in Heliostat::getPanel()");
	}
}

void Heliostat::CopyImageData(const Heliostat *Hsrc){
	/* 
	Copy the image coefficients and data from 'Hsrc' and set as the local heliostat image coefs.

	The following arrays are copied:
	matrix_t<double>
		_mu_MN,		//Normalized mirror shape hermite expansion coefficients (n_terms x n_terms)
		_mu_S,		//Moments of sunshape
		_mu_G,		//Moments of the error distribution
		_mu_M,		//Moments of the mirror shape
		_mu_F,		//Flux moments distrubution - result
		_hcoef,		//Hermite coefficients
		_hc_tht; 		//Hermite coefs depending on tower height - equiv. to mu_F, reused in optimization calcs
	*/
	int nr, nc;

	//_mu_MN
	nr = (int)Hsrc->_mu_MN.nrows();
	nc = (int)Hsrc->_mu_MN.ncols();
	_mu_MN.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_MN.at(i,j) = Hsrc->_mu_MN.at(i,j);
	//_mu_S
	nr = (int)Hsrc->_mu_S.nrows();
	nc = (int)Hsrc->_mu_S.ncols();
	_mu_S.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_S.at(i,j) = Hsrc->_mu_S.at(i,j);
	//_mu_G
	nr = (int)Hsrc->_mu_G.nrows();
	nc = (int)Hsrc->_mu_G.ncols();
	_mu_G.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_G.at(i,j) = Hsrc->_mu_G.at(i,j);
	//_mu_M
	nr = (int)Hsrc->_mu_M.nrows();
	nc = (int)Hsrc->_mu_M.ncols();
	_mu_M.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_M.at(i,j) = Hsrc->_mu_M.at(i,j);
	//_mu_F
	nr = (int)Hsrc->_mu_F.nrows();
	nc = (int)Hsrc->_mu_F.ncols();
	_mu_F.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_F.at(i,j) = Hsrc->_mu_F.at(i,j);
	//_hcoef
	nc = (int)Hsrc->_hcoef.ncells();
	_hcoef.resize(nc);
	for(int j=0; j<nc; j++)
		_hcoef.at(j) = Hsrc->_hcoef.at(j);
	//_hc_tht
	nr = (int)Hsrc->_hc_tht.nrows();
	nc = (int)Hsrc->_hc_tht.ncols();
	_hc_tht.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_hc_tht.at(i,j) = Hsrc->_hc_tht.at(i,j);


}

void Heliostat::getTemplateRange(double &rmin, double &rmax, double &azmin, double &azmax){
	rmin = _temp_rad_min;
	rmax = _temp_rad_max;
	azmin = _temp_az_min;
	azmax = _temp_az_max;
}