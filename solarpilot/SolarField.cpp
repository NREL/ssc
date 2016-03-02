#include "SolarField.h"
#include "math.h"

#include "sort_method.h"
#include "Heliostat.h"
#include "Receiver.h"
#include "Financial.h"
#include "Ambient.h"
#include "Land.h"
#include "Plant.h"
#include "Flux.h"
#include "heliodata.h"

#include "OpticalMesh.h"
#include <algorithm>
#include "exceptions.hpp"

//-------Access functions
//"GETS"
SolarField::clouds *SolarField::getCloudObject(){return &_clouds;}
double SolarField::getDesignPower(){return _q_des;}
int SolarField::getLayoutMethod(){return _layout_method;}
int SolarField::getHelioSortMethod(){return _hsort_method;}
vector<Receiver*> *SolarField::getReceivers(){return &_active_receivers;}
Land *SolarField::getLandObject(){return &_land;}
Ambient *SolarField::getAmbientObject(){return &_ambient;}
Flux *SolarField::getFluxObject(){return _flux;}
Financial *SolarField::getFinancialObject(){return &_financial;}
FluxSimData *SolarField::getFluxSimObject(){return &_fluxsim;}
Plant *SolarField::getPlantObject(){return &_plant;}
htemp_map *SolarField::getHeliostatTemplates(){return &_helio_templates;}
Hvector *SolarField::getHeliostats(){return &_heliostats;}
layout_shell *SolarField::getLayoutShellObject(){return &_layout;}
unordered_map<int,Heliostat*> *SolarField::getHeliostatsByID(){return &_helio_by_id;}
vector<Heliostat> *SolarField::getHeliostatObjects(){return &_helio_objects;}
	
double SolarField::getTowerHeight() {return _tht;}; //[m] Returns optical height at the midpoint of the receiver
double SolarField::getTowerShadowL() {return _towL;}; //[m] Returns the tower shadow base length
double SolarField::getTowerShadowW() {return _towD;}; //[m] Returns the tower shadow width
void SolarField::getLayoutAcceptAngles(double angles[2]){angles[0]=_accept_max; angles[1] = _accept_min;}	//Sets "angles" = [_accept_max, _accept_min]
double SolarField::getDesignPointDNI(){return _dni_des;}
bool SolarField::getAimpointStatus(){return _is_aimpoints_updated;}
double SolarField::getSimulatedPowerToReceiver(){return _sim_p_to_rec;}
int SolarField::getTemplateRule(){return _template_rule;}
double *SolarField::getHeliostatExtents(){return _helio_extents;}
int SolarField::getDesignSimDetail(){return _des_sim_detail;}
WeatherData *SolarField::getSimulationStepData(){return &_sim_step_data;}
	
simulation_info *SolarField::getSimInfoObject(){return &_sim_info;}
simulation_error *SolarField::getSimErrorObject(){return &_sim_error;}
optical_hash_tree *SolarField::getOpticalHashTree(){return &_optical_mesh;}

//-------"SETS"
/*min/max field radius.. function sets the value in units of [m]. Can be used as follows:
1) SetMinFieldRadius(_tht, 0.75); 
2) SetMinFieldRadius(100, 1.0); 
*/
void SolarField::setTowerShadowW(double val){_towD=val;}
void SolarField::setTowerShadowL(double val){_towL=val;}
void SolarField::setTowerHeight(double val){_tht = val;}
void SolarField::setLayoutMethod(int val){_layout_method = val;}
void SolarField::setRadialSpacingMethod(int val){_rad_spacing_method = val;}
void SolarField::setSpacingReset(double val){_spacing_reset = val;}
void SolarField::setInitialSpacing(double val){_az_spacing = val;}
void SolarField::setDesignPower(double val) {if(val>0) {_q_des=val;} };
void SolarField::setAcceptanceAngle(double minmax[2], bool deg){double m=1.; if(deg) m=pi/180.; _accept_min = minmax[0]*m; _accept_max = minmax[1]*m;}
void SolarField::setAimpointStatus(bool state){_is_aimpoints_updated = state;}
void SolarField::setSimulatedPowerToReceiver(double val){_sim_p_to_rec = val;}
void SolarField::setHeliostatExtents(double xmax, double xmin, double ymax, double ymin){ 
	_helio_extents[0] = xmax;
	_helio_extents[1] = xmin;
	_helio_extents[2] = ymax;
	_helio_extents[3] = ymin;
};
	
void SolarField::isPowerOptimized(bool val){_is_power_opt = val;} 		//Vary the power output during optimization to identify optimal level?
bool SolarField::isPowerOptimized(){return _is_power_opt;}
void SolarField::isThtOptimized(bool val){_is_tht_opt = val;}		//Vary the tower height during optimization to identify optimal level?
bool SolarField::isThtOptimized(){return _is_tht_opt;}
void SolarField::isPowerRestricted(bool val){_is_power_restrict = val;}		//Restrict the search range for power output to the indicated limits
bool SolarField::isPowerRestricted(){return _is_power_restrict;}
void SolarField::isThtRestricted(bool val){_is_tht_restrict = val;}		//Restrict the search range for tower height to the indicated limits
bool SolarField::isThtRestricted(){return _is_tht_restrict;}
bool SolarField::isSolarFieldCreated(){return _is_created;}
void SolarField::isOpticalZoning(bool val){_is_opt_zoning = val;}
bool SolarField::isOpticalZoning(){return _is_opt_zoning;}

//Scripts
bool SolarField::ErrCheck(){return _sim_error.checkForErrors();}
void SolarField::CancelSimulation(){
	_cancel_flag = true; 
	_sim_error.addSimulationError("Simulation cancelled by user",true,false);
}
bool SolarField::CheckCancelStatus(){ 
	bool stat = _cancel_flag; 
	//_cancel_flag = false; 
	return stat;} 	//any time this is called, reset the flag since the cancellation will be handled immediately
	

//Constructor / destructor
SolarField::SolarField(){
	//_flux = new Flux();
	_flux = 0;
	_is_created = false;	//The Create() method hasn't been called yet.
	_estimated_annual_power = 0.;
};		//The flux object must have a default constructor associated with the SolarField init method

SolarField::~SolarField(){ 
	if(_flux != (Flux*)NULL) delete _flux; //If the flux object is allocated memory, delete
	//Delete receivers
	for(unsigned int i=0; i<_receivers.size(); i++){
		delete _receivers.at(i);
	}
}

//Copy constructor
SolarField::SolarField( const SolarField &sf )
	: 
	_tht( sf._tht ),
	_accept_max( sf._accept_max ),
	_accept_min( sf._accept_min ),
	_q_des( sf._q_des ), //[MW] Design-point solar field thermal rating
	_dni_des( sf._dni_des ),	//DNI value at which the design-point receiver thermal power is achieved
	_sun_loc_des( sf._sun_loc_des ), 		//Sun location when thermal power rating is achieved
	_sun_loc_des_az( sf._sun_loc_des_az ), 	//Solar azimuth angle at the design point
	_sun_loc_des_el( sf._sun_loc_des_el ), 	//Solar elevation angle at the design point
	_az_spacing( sf._az_spacing ),	//[-] Azimuthal spacing factor of the first row after reset, multiply by heliostat width to get actual spacing
	_spacing_reset( sf._spacing_reset ),	//[-] Heliostat azimuthal spacing ratio.. Indicates how much spacing should expand before starting a new compact row
	_q_to_rec( sf._q_to_rec ),		//[MW] Power to the receiver during performance runs
	_q_des_opt_min( sf._q_des_opt_min ), 		//Minimum possible solar field design power during optimzation
	_q_des_opt_max( sf._q_des_opt_max ),			//Maximum possible solar field design power during optimzation
	_tht_opt_min( sf._tht_opt_min ), 		//Minimum allowable tower height during optimization
	_tht_opt_max( sf._tht_opt_max ), 		//Maximum allowable tower height during optimization
	_sim_time_step( sf._sim_time_step ),		//Simulation weather data time step
	_sim_p_to_rec( sf._sim_p_to_rec ),
	_is_power_opt( sf._is_power_opt ),
	_is_power_restrict( sf._is_power_restrict ),
	_is_tht_restrict( sf._is_tht_restrict ),
	_is_aimpoints_updated( sf._is_aimpoints_updated ),
	_cancel_flag( sf._cancel_flag ),
	_is_created( sf._is_created ),
	_is_opt_zoning( sf._is_opt_zoning ),
	_des_sim_detail( sf._des_sim_detail ),
	_des_sim_ndays( sf._des_sim_ndays ),
	_layout_method( sf._layout_method ),
	_rad_spacing_method( sf._rad_spacing_method ),
	_row_spacing_x(sf._row_spacing_x),		//Separation between adjacent heliostats in the X-direction, multiplies heliostat radius
	_row_spacing_y(sf._row_spacing_y),		//Separation between adjacent heliostats in the Y-direction, multiplies heliostat radius
	_xy_rect_aspect(sf._xy_rect_aspect),		//Aspect ratio of the rectangular field layout (height in Y / width in X)
	_xy_field_shape(sf._xy_field_shape),
	_template_rule( sf._template_rule ),
	_layout_data( sf._layout_data ),
	_des_sim_nhours( sf._des_sim_nhours),
	_sim_step_data( sf._sim_step_data ),	//Copy constructor handles resetting pointers
	_layout( sf._layout ),
	_helio_objects( sf._helio_objects ),	//This contains the heliostat objects. The heliostat constructor will handle all internal pointer copy operations
	_helio_template_objects( sf._helio_template_objects ),	//This contains the heliostat template objects.
	_hsort_method( sf._hsort_method),
	_sim_info( sf._sim_info ),
	_sim_error( sf._sim_error ),
	_is_prox_filter( sf._is_prox_filter ),
	_prox_filter_frac( sf._prox_filter_frac ),
	_max_zone_size_rad( sf._max_zone_size_rad),
	_max_zone_size_az( sf._max_zone_size_az ),
	_min_zone_size_rad( sf._min_zone_size_rad ),
	_min_zone_size_az( sf._min_zone_size_az ),
	_zone_div_tol( sf._zone_div_tol ),
	_estimated_annual_power( sf._estimated_annual_power ),
	_q_des_withloss( sf._q_des_withloss ),
    _temp_which( sf._temp_which ),
    _trans_limit_fact( sf._trans_limit_fact),

	//classes
	_ambient( sf._ambient ),
	_land( sf._land ),
	_financial( sf._financial ),
    _fluxsim( sf._fluxsim ),
	_plant( sf._plant )
{
	//------- Reconstruct pointer maps, etc ----------
	for(int i=0; i<4; i++){
		_helio_extents[i] = sf._helio_extents[i]; 
	}
	//Create a map between the original heliostat object addresses and the new address
	unordered_map<Heliostat*, Heliostat*> hp_map;
	for(unsigned int i=0; i<_helio_objects.size(); i++){
		hp_map[ const_cast<Heliostat*>( &sf._helio_objects.at(i) ) ] = &_helio_objects.at(i);
	}

	//Create a temporary pointer map between the old and new heliostat template objects
	unordered_map<Heliostat*,Heliostat*> htemp_ptr_map;
	for(int i=0; i<(int)_helio_template_objects.size(); i++){	
		htemp_ptr_map[ const_cast<Heliostat*>( &(sf._helio_template_objects.at(i)) ) ] = &(_helio_template_objects.at(i));
	}

	//Reassign the integer->Heliostat* map
	
	_helio_templates.clear();
	for(unsigned int i=0; i<_helio_template_objects.size(); i++){
		_helio_templates[i] = &_helio_template_objects.at(i);
	}
	//Set up the heliostat pointer array
	/* 
	The ID's are originally set according to the position in the heliostat object matrix.
	Use this feature to reconstruct the pointer map.
	*/
	int npos = sf._heliostats.size();
	_heliostats.resize(npos);
	for(int i=0; i<npos; i++){
		_heliostats.at(i) = hp_map[ sf._heliostats.at(i) ];
		_heliostats.at(i)->setMasterTemplate( htemp_ptr_map[ _heliostats.at(i)->getMasterTemplate()] );
	}
	_helio_by_id.clear();
	for(unsigned int i=0; i<sf._helio_by_id.size(); i++){	//Do a separate loop for ID's since they are not necessarily assigned right away
		int id = sf._heliostats.at(i)->getId();
		_helio_by_id[ id ] = hp_map[ const_cast<Heliostat*>( const_cast<SolarField*>(&sf)->_helio_by_id[id] ) ];
	}

	//Heliostat groups
	int nr, nc;
	nr = sf._helio_groups.nrows();
	nc = sf._helio_groups.ncols();
	_helio_groups.resize_fill(nr, nc, Hvector()); //NULL));
	for(int i=0; i<nr; i++){
		for(int j=0; j<nc; j++){
			int nh = sf._helio_groups.at(i,j).size();
			_helio_groups.at(i,j).resize(nh);
			for(int k=0; k<nh; k++){
				_helio_groups.at(i,j).at(k) = hp_map[ sf._helio_groups.at(i,j).at(k) ];
			}
		}
	}

	//Layout groups
	_layout_groups.resize(sf._layout_groups.size());
	for(int j=0; j<(int)sf._layout_groups.size(); j++){
		int nh = sf._layout_groups.at(j).size();
		_layout_groups.at(j).resize(nh);
		for(int k=0; k<nh; k++){
			_layout_groups.at(j).at(k) = hp_map[ sf._layout_groups.at(j).at(k) ];
		}
	}
	

	//Neighbors
	nr = sf._neighbors.nrows();
	nc = sf._neighbors.ncols();
	_neighbors.resize_fill(nr, nc, Hvector()); //0, NULL));
	for(int i=0; i<nr; i++){
		for(int j=0; j<nc; j++){
			int nh = sf._neighbors.at(i,j).size();
			_neighbors.at(i,j).resize(nh);
			for(int k=0; k<nh; k++){
				_neighbors.at(i,j).at(k) = hp_map[ sf._neighbors.at(i,j).at(k)];
			}
		}
	}
	for(int i=0; i<npos; i++){
		//Each heliostat needs to have the neighbor group assigned to it
		Heliostat *hptr = _heliostats.at(i);
		hptr->setNeighborList( &_neighbors.at( hptr->getGroupId()[0], hptr->getGroupId()[1] ) );
	}

	//Create receivers
	unordered_map<Receiver*, Receiver*> r_map;	//map old receiver -> new receiver
	int nrec = sf._receivers.size();
	_active_receivers.clear();
	for(int i=0; i<nrec; i++){
		Receiver *rec = new Receiver( *sf._receivers.at(i) );
		_receivers.push_back( rec );
		r_map[ sf._receivers.at(i) ] = rec;		//map
		//When copying the receiver class, update the flux surfaces to point to the correct receiver
		FluxSurfaces *fs = rec->getFluxSurfaces();
		for(unsigned int j=0; j<fs->size(); j++){
			fs->at(j).setParent( rec );
		}
		//Recreate the active receivers list
		if(rec->isReceiverEnabled())
			_active_receivers.push_back( rec );
	}


	//Assign the correct receiver to each heliostat
	for(int i=0; i<npos; i++){
		_heliostats.at(i)->setWhichReceiver( r_map[ _heliostats.at(i)->getWhichReceiver() ] );
	}

	//Flux
	//if(_flux != (Flux*)NULL ) delete _flux;
	_flux = new Flux( *sf._flux );

}


//Scripts
void SolarField::Create(var_set &V, int var_index){
	/*
	Create a solar field instance, set the variable values according to the map provided by the GUI.
	This simply instantiates all the needed objects for the solar field but does not do any analysis.
	*/
	_sim_info.addSimulationNotice("Creating solar field geometry");

	//Clean out possible existing variables
	Clean();
	
	//Create the flux object
	if(_flux != 0 ){ delete _flux; }	//If the flux object already exists, delete and start over
	_flux = new Flux();
	_flux->Setup();
	
	//Go through and set all of the relevant variables, constructing objects as needed along the way.
	
	//Solar field variables
	setVar("accept_max", _accept_max, V["solarfield"][var_index], 180., "[-180.,180.]");		//Upper bound of the angular range containing the heliostat field
	setVar("accept_min", _accept_min, V["solarfield"][var_index], -180., "[-180.,180.]");		//Lower bound of the angular range containing the heliostat field
	setVar("q_des", _q_des, V["solarfield"][var_index], 500., "(0.,99999.]");		//Design thermal power delivered from the solar field
	setVar("q_des_opt_min", _q_des_opt_min, V["solarfield"][var_index], 100., "(0.,99999.]");		//Minimum possible solar field design power during optimzation
	setVar("q_des_opt_max", _q_des_opt_max, V["solarfield"][var_index], 1000., "(0.,99999.]");		//Maximum possible solar field design power during optimzation
	setVar("dni_des", _dni_des, V["solarfield"][var_index], 950., "(0.,1300.]");		//DNI value at which the design-point receiver thermal power is achieved
	setVar("sun_loc_des", _sun_loc_des, V["solarfield"][var_index], 0);		//Sun location when thermal power rating is achieved
	setVar("sun_loc_des_az", _sun_loc_des_az, V["solarfield"][var_index], 180., "[-180.,180.]");		//Solar azimuth angle at the design point
	setVar("sun_loc_des_el", _sun_loc_des_el, V["solarfield"][var_index], 85., "[0.,90]");		//Solar elevation angle at the design point
	setVar("az_spacing", _az_spacing, V["solarfield"][var_index], 2., "[1.,1000.]");		//Azimuthal spacing factor for the first row of heliostats after a reset. Heliostats separated by heliostat width times this factor.
	setVar("spacing_reset", _spacing_reset, V["solarfield"][var_index], 1.33, "(1.,1000.]");		//For heliostat layout - ratio of maximum to initial azimuthal spacing before starting new compressed row
	setVar("trans_limit_fact", _trans_limit_fact, V["solarfield"][var_index], 0.92, "[0,999]");		//Determines the point at which close-packing switches to standard layout. =1 at no-blocking transition limit.
    setVar("layout_method", _layout_method, V["solarfield"][var_index], 1, "[0,99]");		//Field layout method
	setVar("layout_data", _layout_data, V["solarfield"][var_index], "");		//Layout data in string form
	setVar("max_zone_size_rad", _max_zone_size_rad, V["solarfield"][var_index], 1., "(0,100]");		//Maximum zone size (radial direction) for grouping optical intercept factor calculations
	setVar("max_zone_size_az", _max_zone_size_az, V["solarfield"][var_index], 1., "(0,100]");		//Maximum zone size (azimuthal direction) for grouping optical intercept factor calculations
	setVar("min_zone_size_rad", _min_zone_size_rad, V["solarfield"][var_index], 0.1, "(0,100]");		//Minimum zone size (radial direction) for grouping optical intercept factor calculations
	setVar("min_zone_size_az", _min_zone_size_az, V["solarfield"][var_index], 0.1, "(0,100]");		//Minimum zone size (azimuthal direction) for grouping optical intercept factor calculations
	setVar("zone_div_tol", _zone_div_tol, V["solarfield"][var_index], 0.001, "[0.001,1]");		//Allowable variation in optical intercept factor within a layout zone
	setVar("is_opt_zoning", _is_opt_zoning, V["solarfield"][var_index], true);		//Enables grouping of heliostats into zones for intercept factor calculation during layout only
	setVar("rad_spacing_method", _rad_spacing_method, V["solarfield"][var_index], 2, "[1,2]");		//Method for determining radial spacing during field layout for radial-stagger
	setVar("row_spacing_x", _row_spacing_x, V["solarfield"][var_index], 1.1, "(1,1000.]");		//Separation between adjacent heliostats in the X-direction, multiplies heliostat radius
	setVar("row_spacing_y", _row_spacing_y, V["solarfield"][var_index], 1.1, "(1,1000.]");		//Separation between adjacent heliostats in the Y-direction, multiplies heliostat radius
	setVar("xy_field_shape", _xy_field_shape, V["solarfield"][var_index], 0, "[0,2]");		//Enforced shape of the heliostat field
	setVar("xy_rect_aspect", _xy_rect_aspect, V["solarfield"][var_index], 1., "(0.,1000.]");		//Aspect ratio of the rectangular field layout (height in Y / width in X)
	setVar("is_prox_filter", _is_prox_filter, V["solarfield"][var_index], false);		//Post-process the layout to select heliostats that are closer to the tower.
	setVar("prox_filter_frac", _prox_filter_frac, V["solarfield"][var_index], 0.03, "[0.,1)");		//Fraction of heliostats to subject to proximity filter.
	setVar("des_sim_detail", _des_sim_detail, V["solarfield"][var_index], 5, "[0,4]");		//Simulation detail for placing heliostats (see definitions in options spreadsheet)
	setVar("des_sim_ndays", _des_sim_ndays, V["solarfield"][var_index], 4, "(1,365]");		//For limited annual simulation, the number of evenly spaced days to simulate
	setVar("des_sim_nhours", _des_sim_nhours, V["solarfield"][var_index], 2, "[1,5]");		//Simulation will run with the specified hourly frequency (1=every hour, 2=every other hour...)
	setVar("sim_step_data", _sim_step_data, V["solarfield"][var_index]);		//Data used for design simulations
	setVar("sim_time_step", _sim_time_step, V["solarfield"][var_index], 3600.);		//Simulation weather data time step
	setVar("tht", _tht, V["solarfield"][var_index], 180., "(0.,1000.]");		//Average height of the tower receiver centerline above the base heliostat pivot point elevation
	setVar("tht_opt_min", _tht_opt_min, V["solarfield"][var_index], 125., "(0.,1000.]");		//Minimum allowable tower height during optimization
	setVar("tht_opt_max", _tht_opt_max, V["solarfield"][var_index], 225., "(0.,1000.]");		//Maximum allowable tower height during optimization
	setVar("shadow_width", _towD, V["solarfield"][var_index], 16., "(0.,1000.]");		//Effective tower diameter for shadowing calculations
	setVar("shadow_height", _towL, V["solarfield"][var_index], _tht, "(0.,1000.]");		//Effective tower height for shadowing calculations
	setVar("is_power_opt", _is_power_opt, V["solarfield"][var_index], false);		//Vary the power output during optimization to identify optimal level?
	setVar("is_tht_opt", _is_tht_opt, V["solarfield"][var_index], true);		//Vary the tower height during optimization to identify optimal level?
	setVar("is_power_restrict", _is_power_restrict, V["solarfield"][var_index], false);		//Restrict the search range for power output to the indicated limits
	setVar("is_tht_restrict", _is_tht_restrict, V["solarfield"][var_index], true);		//Restrict the search range for tower height to the indicated limits
	setVar("template_rule", _template_rule, V["solarfield"][var_index], 0);		//Method for distributing heliostat geometry templates in the field
	setVar("temp_which", _temp_which, V["solarfield"][var_index], 0);		//Select the heliostat geometry template that will be used in the layout
    setVar("hsort_method", _hsort_method, V["solarfield"][var_index], 7);		//Select the criteria by which heliostats will be included in the solar field layout.

	//Unit conversions
	_accept_max *= d2r;
	_accept_min *= d2r;
	
	setAimpointStatus(false);	//the aimpoints have not yet been calculated
	
	//-- Heliostat variables --
    //establish template order, if needed
    vector<int> 
        tnum,
        tord;
    int i=0;
    //create vectors of the template numbers and template orders
	for(map<int, var_map>::iterator it=V["heliostat"].begin(); it != V["heliostat"].end(); it++){
        tnum.push_back(it->first);
        tord.push_back(it->second["template_order"].value_int() );
    }
    //Sort by template order, correlate with template number vector
    if(tord.size() > 1 )
        quicksort(tord, tnum, 0, (int)tord.size()-1 );

    //Instantiate the template objects in order
	i=0;
    int nh = V["heliostat"].size();
	_helio_template_objects.resize(nh);
    for(int j=0; j<nh; j++)
    {
		_helio_template_objects.at(i).Create(V["heliostat"][ tnum.at(j) ]);
        _helio_template_objects.at(i).setType( tnum.at(j) ); //assign the template type integer
		_helio_templates[ i ] = &_helio_template_objects.at(i);
		i++;
	}
	
	//Parse the layout string into a layout object
	if(_layout_data != ""){
		//Convert the string contents to a layout_shell object
		SolarField::parseHeliostatXYZFile( _layout_data, _layout );
	}

	//Receiver variables
	int Nset = V["receiver"].size();
	i=0;
	_active_receivers.clear();
	for(map<int, var_map>::iterator it=V["receiver"].begin(); it != V["receiver"].end(); it++){
		Receiver *rec = new Receiver();
		_receivers.push_back( rec );
		_receivers.at(i)->Create(V["receiver"][ it->first ]);
		if(_receivers.back()->isReceiverEnabled())
			_active_receivers.push_back(rec);
		i++;
	}
	
	//Ambient
	_ambient.Create(V["ambient"][0]);
	_ambient.setSimTimeStep( _sim_time_step );	//Map the time step to the ambient object

	//Land
	_land.Create(V["land"][0]);

	//Cost - financial
	_financial.Create(V["financial"][0]);

	//Plant
	_plant.Create(V["plant"][0]);

	//Clouds
	double ext[2];
	_land.getExtents(ext, _tht);
	_clouds.Create(V["fluxsim"][0], ext);

    //Fluxsim
    _fluxsim.Create(V["fluxsim"][0]);

	_is_created = true;
	
}

void SolarField::Clean(){
	/* 
	Clean out existing solar field and subcomponent arrays for a new start. These are the 
	variables that aren't set in the "Create()" methods.
	*/

	for(int i=0; i<4; i++) _helio_extents[i] = 0.;
	_layout.clear();
	_helio_objects.clear();
	_helio_templates.clear();
	_heliostats.clear();
	_helio_groups.clear();
	_helio_by_id.clear();
	_neighbors.clear();
	_receivers.clear();
	
	_ambient.Clean();
	_land.Clean();
	_financial.Clean();
	_plant.Clean();
	_is_created = false;
	_cancel_flag = false;	//initialize the flag for cancelling the simulation
	_optical_mesh.reset();

}

void SolarField::getTowerShadow(double ShadWL[2]){
	//Returns the tower shadow array [Width,Length]
	ShadWL[0]=_towD; 
	ShadWL[1]=_towL; 
};

void SolarField::setTowerShadow(double arr[2]){
	//Take the Length=2 array with [Width,Height] and assign it to the class members (shadow_w, shadow_l)
	_towD = arr[0];
	_towL = arr[1];
	return;
}

double SolarField::getHeliostatArea(){
	/* Sum up the total aperture area of all active heliostats in the solar field */
	int Npos = _heliostats.size();
	double Asf=0.;
	for(int i=0; i<Npos; i++){
		if(_heliostats.at(i)->getInLayout()) Asf += _heliostats.at(i)->getArea();
	}
	
	return Asf;
}

double SolarField::getReceiverTotalArea(){
	/* 
	Sum and return the total absorber surface area for all receivers
	*/

	int nrec = getReceivers()->size();
	double Atot = 0.;
	for(int i=0; i<nrec; i++){ 
		if(! getReceivers()->at(i)->isReceiverEnabled() ) continue;
		Atot += getReceivers()->at(i)->getReceiverAbsorberArea(); 
	}
	return Atot;
}

bool SolarField::UpdateNeighborList(double lims[4], double zen){
	/* 
	Update the neighbors associated with each heliostat based on the shadow extent. Determine this range
	based on the current sun position

	lims:
	0 : xmin
	1 : xmax
	2 : ymin
	3 : ymax
	*/
	
	double 
		xmax = lims[0],
		xmin = lims[1],
		ymax = lims[2],
		ymin = lims[3];

	//add a little buffer to the min/max extents
	if(xmax>0.) {xmax *= 1.01;} else {xmax *= 0.99;}
	if(xmin<0.) {xmin *= 1.01;} else {xmin *= 0.99;}
	if(ymax>0.) {ymax *= 1.01;} else {ymax *= 0.99;}
	if(ymin<0.) {ymin *= 1.01;} else {ymin *= 0.99;}
	
	//How many nodes should be in the mesh? Determine the shadow length at some low sun position (say 15deg)
	//and use this to size the nodes. Shadowing will be evaluated among heliostats in the current node
	//and surrounding 8 nodes.
	double rcol = 0.;
	double hm = 0.;
	for(htemp_map::iterator it = _helio_templates.begin(); it != _helio_templates.end(); it++){
		rcol += it->second->getCollisionRadius();
		hm += it->second->getHeight()/2.;
	}
	rcol *= 1./(double)_helio_templates.size();
	hm *= 1./(double)_helio_templates.size();

	// This is the shadowing radius at 15deg. Use this as the x and y dimension of the mesh.
	// Multiply the cell width by 0.5 since several adjacent cells will participate in the shadowing. We don't
	// need each cell to be the full shadowing radius wide.
	double r_shad_max = fmax(2.*rcol/tan(pi/2.-zen), 3.*rcol);
	double r_block_max = 10.*hm;	//by similar triangles and assuming the maximum heliostat-to-tower distance is 10 tower heights, calculate max. blocking interaction radius
	double r_interact = max(r_shad_max, r_block_max);
	r_interact = fmin(r_interact, 2.*hm*100);	//limit to a reasonable value
	int ncol, nrow;
	double dcol, drow;
	ncol = max(1, int((xmax - xmin)/r_interact));		//The number of column nodes
	nrow = max(1, int((ymax - ymin)/r_interact));		//The number of row nodes
	dcol = (xmax - xmin)/float(ncol);
	drow = (ymax - ymin)/float(nrow);			//The column and row node width

	//resize the mesh array accordingly
	_helio_groups.resize_fill(nrow, ncol, Hvector());

	int col, row;	//indicates which node the heliostat is in
	int Npos = _helio_objects.size();
	for(int i=0; i<Npos; i++){
		Heliostat *hptr = &_helio_objects.at(i);
		//Find which node to add this heliostat to
		row = (int)(floor((hptr->getLocation()->y - ymin)/drow));
		row = (int)fmax(0., fmin(row, nrow-1));
		col = (int)(floor((hptr->getLocation()->x - xmin)/dcol));
		col = (int)fmax(0., fmin(col, ncol-1));
		//Add the heliostat ID to the mesh node
		_helio_groups.at(row,col).push_back(hptr); 
		//Add the mesh node ID to the heliostat information
		hptr->setGroupId(row,col);
	}

	//Go over each node and compile a list of neighbors from the block of 9 adjacent cells.
	if(CheckCancelStatus()) return false;	//check for cancelled simulation
	int nh;
	_neighbors.resize_fill(nrow, ncol, Hvector());
	for(int i=0; i<nrow; i++){		//Loop over each row
		for(int j=0; j<ncol; j++){	//Loop over each column
			for(int k=i-1;k<i+2;k++){	//At each node position, get the surrounding nodes in the +/-y direction
				if(k<0 || k>nrow-1) continue;	//If the search goes out of bounds in the y direction, loop to the next position
				for(int l=j-1;l<j+2;l++){	//At each node position, get the surrounding nodes in the +/-x direction
					if(l<0 || l>ncol-1) continue;	//If the search goes out of bounds in the x direction, loop to the next position
					nh = _helio_groups.at(k,l).size();	//How many heliostats are in this cell?
					for(int m=0; m<nh; m++){	//Add each heliostat element in the mesh to the list of neighbors for the current cell at (i,j)
						_neighbors.at(i,j).push_back( _helio_groups.at(k,l).at(m) );
					}
				}
			}
		}
	}
	if(CheckCancelStatus()) return false;	//check for cancelled simulation

	//For each heliostat, assign the list of neighbors that are stored in the _neighbors grid
	for(int i=0;i<Npos;i++){
		Heliostat *hptr = &_helio_objects.at(i);
		hptr->setNeighborList( &_neighbors.at( hptr->getGroupId()[0], hptr->getGroupId()[1] ) );	//Set the neighbor list according to the stored _neighbors indices
	}
	return true;

}

bool SolarField::UpdateLayoutGroups(double lims[4]){
	
	/* 
	Create an "optical mesh" of the field based on changes in the local view factor and
	intercept factor (approximate) throughout the field. Certain areas in the heliostat
	field will experience more rapid rates of change of optical performance, and we can
	choose the grouping of heliostats carefully to minimize the number of required zone
	calculations while making sure that we don't group together heliostats that are ac-
	tually different in performance.

	The method here uses the derivative of intercept factor and of view factor to dete-
	rmine the allowable grid size. Each region is evaluated and divided 'n' times until
	the change in the efficiency over the azimuthal and radial spans of the zone falls
	below the allowed tolerance. 

	Zones are assigned a unique binary code that represents their position in the field.
	The code is determined by dividing each variable space in half, and 0 corresponds 
	to the lower region while 1 is the upper. Divisions are made alternatingly between
	radial (first) and azimuthal dimensions. Once the division has reached satisfactory
	resolution in one dimension, subsequent potential divisions are denoted with 'x', as
	no division actually takes place. The remaining dimension continues division until
	the required tolerance is met. At the end of the division chain, the flat 't' indi-
	cates that the node is terminal and contains data.

	For example:
					10001x1xt
	represents:
	[r1]	outer
	[a1]	ccw
	[r2]	inner
	[a2]	ccw
	[r3]	outer
	[a3]	no division
	[r4]	outer
	[a4]	no division
	terminal

	In words, 
		(1) divide a circle into outer and inner radial sections - take the outer,
		(2) divide region 1 into two slices (halves) - take the counterclockwise one,
		(3) divide region 2 into outer and inner radial sections - take the inner,
	....
		the terminal node contains an array of data (heliostat pointers).

	Likewise, each heliostat can be assigned a unique binary tag using this method. 
	Simply take the location of the heliostat and place it within the domain by subse-
	quent division. Continue until both 'r' and 'az' dimensions have sufficient resol-
	ution. The heliostat key will be entirely comprised of 0's and 1's.

	A heliostat can be placed in a zone by comparing the zone and heliostat location keys
	bitwise until the termination flag is reached. Where the zone key has an 'x', the
	zone does not split, and so the heliostat will continue down the mesh tree on the
	only available path. Once the 't' flag is reached, the heliostat is dropped into the
	data container.


	This method has been implemented as a means for quickly sorting a large number of 
	heliostats that are known in x-y coordinates into a grid with variable mesh spacing
	that is defined in cylindrical coordinates. This method can probably be applied to
	sorting in other coordinate systems, but it's application here is specific to this
	problem.
	*/

	double 
		xmax = lims[0],
		xmin = lims[1],
		ymax = lims[2],
		ymin = lims[3];

	//add a little buffer to the min/max extents
	xmax = xmax > 0. ? xmax * 1.01 : xmax * 0.99;
	xmin = xmin > 0. ? xmin * 1.01 : xmin * 0.99;
	ymax = ymax > 0. ? ymax * 1.01 : ymax * 0.99;
	ymin = ymin > 0. ? ymin * 1.01 : ymin * 0.99;


	//create objects needed 
	LayoutData mesh_data;
	mesh_data.extents_az[0] = _accept_min;
	mesh_data.extents_az[1] = _accept_max;
	mesh_data.tht = _tht;
	mesh_data.alpha = _receivers.front()->getReceiverAzimuth();
	mesh_data.theta = _receivers.front()->getReceiverElevation();
	//double width;
	Receiver *rec = _receivers.front();
	mesh_data.w_rec = _receivers.front()->getReceiverWidth(); //sqrt(powf(_receivers.front()->getReceiverWidth(),2) + powf(_receivers.front()->getReceiverHeight(),2));
	mesh_data.flat = _receivers.front()->getReceiverGeomType() != Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED
					&& _receivers.front()->getReceiverGeomType() != Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN;
	mesh_data.f_tol = _zone_div_tol;
	mesh_data.max_zsize_a = _max_zone_size_az*_tht;
	mesh_data.max_zsize_r = _max_zone_size_rad*_tht;
	mesh_data.min_zsize_a = _min_zone_size_az*_tht;
	mesh_data.min_zsize_r = _min_zone_size_rad*_tht;
	
	//mesh separately for each heliostat template
	int ntemp = (int)_helio_templates.size();
	vector<vector<opt_element> > all_nodes(ntemp);
	_layout_groups.clear();

	for(htemp_map::iterator it=_helio_templates.begin(); it!=_helio_templates.end(); it++){
		
		double trange[2], arange[2];
		TemplateRange(it->first, _template_rule, trange, arange);
		mesh_data.extents_r[0] = trange[0];
		mesh_data.extents_r[1] = trange[1];
		mesh_data.extents_az[0] = arange[0];
		mesh_data.extents_az[1] = arange[1];
		
		int fmethod = it->second->getFocusMethod();
		mesh_data.onslant = fmethod == 1;
		switch(fmethod)
		{
		case 0:
			mesh_data.L_f = 1.e9;	//infinite
			break;
		case 1:
			break;	//on slant. this is handled internally
		case 2:
			//average of group
			mesh_data.L_f = (trange[0] + trange[1])/2.;
			break;
		case 3:
			//user specified
			mesh_data.L_f = sqrt(pow(it->second->getFocalX(),2) + pow(it->second->getFocalY(),2));
			break;
		};
		
		mesh_data.H_h = it->second->getHeight();
		mesh_data.H_w = it->second->getWidth();
		if( it->second->IsFacetDetail() ){
			mesh_data.nph = it->second->getNumCantY();
			mesh_data.npw = it->second->getNumCantX();
		}
		else{
			mesh_data.nph = mesh_data.npw = 1;
		}
		//calculate the total reflected beam error distribution
		double err[2], errnorm=0., errsurf;
		it->second->getErrorAngular(err);
		errnorm = err[0]*err[0] + err[1]*err[1];
		it->second->getErrorSurface(err);
		errnorm += err[0]*err[0] + err[1]*err[1];
		it->second->getErrorReflected(err);
		errsurf = err[0]*err[0] + err[1]*err[1];	
		mesh_data.s_h = sqrt(4.*errnorm + errsurf);

		//Set the maximum error in the binary tag location
		mesh_data.t_res = fmin(mesh_data.H_h, mesh_data.H_w)/10.;   

		//Create the mesh
		_optical_mesh.reset();
		_optical_mesh.create_mesh(&mesh_data);

		//Add all of the heliostats with this template type to the mesh
		for( vector<Heliostat>::iterator hit = _helio_objects.begin(); hit != _helio_objects.end(); hit++){
			if( hit->getMasterTemplate() != it->second ) continue;
			Point *loc = hit->getLocation();
			_optical_mesh.add_object( &(*hit), loc->x, loc->y);
		}

		//Now add all of the layout groups with heliostats to the main array
		vector<vector<void* >* > tgroups = _optical_mesh.get_terminal_data();
		for(int i=0; i<(int)tgroups.size(); i++){
			int ntgroup = tgroups.at(i)->size();
			if(ntgroup == 0) continue;
			_layout_groups.push_back(Hvector());
			for(int j=0; j<ntgroup; j++){
				_layout_groups.back().push_back( (Heliostat*)tgroups.at(i)->at(j) );
			}
		}
	}
	//report to the log window the heliostat simulation reduction ratio
	char msg[200];
	sprintf(msg, "Identified %d optical zones (%.1f avg size)", 
		(int)_layout_groups.size(), (double)_heliostats.size()/(double)_layout_groups.size());
	_sim_info.addSimulationNotice(msg);

	if(CheckCancelStatus()) return false;	//check for cancelled simulation

	return true;

}

bool SolarField::FieldLayout(){
	/* 
	This should only be called by the API. If using the GUI, manually call PrepareFieldLayout(), 
	DoLayout(), and ProcessLayoutResults() from the interface. This call is not capable of 
	multithreading.
	*/
	WeatherData wdata; //Weather data object will be filled in PrepareFieldLayout(...)
	bool needs_sim = PrepareFieldLayout(*this, wdata);
	
	if( needs_sim){
		//vector<double> results;
		sim_results results;
		int sim_first, sim_last;
		sim_first = 0;
		sim_last = wdata.DNI.size();
		if(! DoLayout(this, &results, &wdata, sim_first, sim_last) )
            return false;

		//For the map-to-annual case, run a simulation here
		if(_des_sim_detail == LAYOUT_DETAIL::MAP_TO_ANNUAL)
			SolarField::AnnualEfficiencySimulation(_ambient.getWeatherFilePath(), this, results); //, (double*)NULL, (double*)NULL, (double*)NULL);


		ProcessLayoutResults(&results, sim_last - sim_first);
	}

    return true;

}

bool SolarField::PrepareFieldLayout(SolarField &SF, WeatherData &wdata, bool refresh_only){
	/*
	This algorithm is used to prepare the solar field object for layout simulations.
	A simulation is performed in this algorithm only if loading specified coordinates
	or if the heliostats are not filtered by performance.

	After calling this method, call DoLayout(...) to run the simulation for
	the given weather data steps, then call ProcessLayoutResults(...) to filter the
	heliostats by performance.
	
	This method uses set geometry values (no optimization within this algorithm), 
	so receiver, heliostat, and tower height geometries should be specified a priori.

	This method can also be called in "refresh_only=true" mode, where solar field 
	geometry parameters are updated without recalculating the heliostat positions.


	*****Procedure for laying out the field**************

	-> Choose a tower height and receiver geometry
	-> Determine all of the available positions for heliostats
		- Enforce any land constraints
	-> Create an unordered_map of heliostats to fill these positions
		- Update each heliostat object with the appropriate slant range
		- Update each heliostat's tracking vector to the time/day desired for canting (if applicable)
		- "Install" the panels on each heliostat, assigning canting and focal point
	-> Analyze each heliostat for optical efficiency at a range of timesteps
	-> Apply a filtering algorithm to determine which heliostats should be removed
	-> Do an annual performance run to find total productivity
	-> Calculate the estimated project IRR
		- Include market factors
	-> Adjust independent variables and repeat the process to find optimal system

	*****************************************************
	
	*/
	
	if(! SF.getSimInfoObject()->addSimulationNotice("Generating solar field heliostat layout") )
    {
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation

	//variables
	vector<Point> HelPos;		//Vector pointer for heliostat positions
	
	//If the analysis method is Hermite Polynomial, initialize polynomial coefs
	Ambient *ambient = SF.getAmbientObject();
	SF.getFluxObject()->initHermiteCoefs( *ambient );
	//Calculate the Hermite geometry coefficients for each template
	for(htemp_map::iterator htemp=SF.getHeliostatTemplates()->begin(); 
                            htemp != SF.getHeliostatTemplates()->end(); 
                            htemp++)
    {
        if( htemp->second->IsEnabled() )
		    SF.getFluxObject()->hermiteMirrorCoefs(*htemp->second, SF.getTowerHeight() );
	}

	//Calculate available heliostat positions
	SF.getSimInfoObject()->addSimulationNotice("Calculating available heliostat positions");
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation
	int layout_method = SF.getLayoutMethod();
	layout_shell *layout = SF.getLayoutShellObject();
	if(!refresh_only && layout_method == 1){		//Radial stagger
		SF.radialStaggerPositions(HelPos);
	}
	else if(!refresh_only && layout_method == 2){	//Cornfield rows (eSolar type)
		SF.cornfieldPositions(HelPos);
	}
	else if(layout_method == 3 || refresh_only){	//User-defined field
		/*
		Take a previously defined layout and build up the heliostat objects
		*/
		int nh = layout->size();	//Number of heliostats

		//Resize the HelPos array
		HelPos.resize(nh);

		//Assign the xyz location.. leave assignment of the other information provided in the user
		//data for later.
		for(int i=0; i<nh; i++){
			HelPos.at(i) = layout->at(i).location;
		}

	}
	
	//Weed out the heliostats that lie outside of the land boundary
	//Exclude any points that aren't within the land boundaries
    if(! SF.getSimInfoObject()->addSimulationNotice("Checking for land boundary exclusions") ){
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation
	if(SF.getLandObject()->isBoundsArray()){
		vector<int> dels;
		//Find which points lie outside the bounds
		for(unsigned int j=0; j<HelPos.size(); j++){
			if(! SF.getLandObject()->InBounds( HelPos.at(j), SF.getTowerHeight()) ){ dels.push_back(j); }
		}
		//Delete in reverse order
		int nd = dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}

        //check for problems here
        if(HelPos.size() == 0 )
            throw spexception("The specified land boundaries resulted in an infeasible design. "
                              "No heliostats could be placed in the layout set. Please review "
                              "your settings for land constraints.");
	}
    /* 
    
    enforce other exclusions (receiver acceptance, receiver cylinder
    
    */

    //receiver diameter - delete heliostats that fall within the diameter of the receiver
    if(SF.getReceivers()->size() == 1 
        && SF.getReceivers()->front()->getReceiverType() == Receiver::REC_TYPE::CYLINDRICAL)
    {
        vector<int> dels;
        for(size_t j=0; j<HelPos.size(); j++){
            double x = HelPos.at(j).x;
            double y = HelPos.at(j).y;
            double h_rad = sqrt(x*x + y*y);

            if( h_rad < SF.getReceivers()->front()->getReceiverWidth()/2.)
                dels.push_back(j);                        
        }
        //Delete in reverse order
		int nd = dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}
    }
    //Receiver span angles
    if(SF.getReceivers()->size() == 1){
        vector<int> dels;
        //receiver acceptance angle
        Receiver *Rec = SF.getReceivers()->front();

        int rectype = Rec->getReceiverType();
        int j=0;
        for(vector<Point>::iterator hpos = HelPos.begin(); hpos != HelPos.end(); hpos++){

            if(rectype == Receiver::REC_TYPE::FLAT_PLATE || rectype == Receiver::REC_TYPE::CAVITY){
		        PointVect rnv;
		        Rec->CalculateNormalVector(rnv);

                //calculate the vector from the receiver to the heliostat
                Point offset;
                double tht = Rec->getOpticalHeight(); //SF.getTowerHeight();
                Rec->getReceiverOffset(offset);
                Vect hv_r;
                hv_r.i = hpos->x - offset.x;
                hv_r.j = hpos->y - offset.y;
                hv_r.k = hpos->z - tht;
                Toolbox::unitvect(hv_r);

                //Rotate into receiver aperture coordinates
		        double raz = Rec->getReceiverAzimuth();
		        double rel = Rec->getReceiverElevation();
		        
                Toolbox::rotation(SF.pi - raz, 2, hv_r);
		        Toolbox::rotation(SF.pi - rel, 0, hv_r);

		        double theta_x = atan2(hv_r.i, hv_r.j);
		        double theta_y = atan2(hv_r.k, sqrt(hv_r.i*hv_r.i + hv_r.j*hv_r.j));

		        //check whether the angles are within the allowable range
		        double acc_x, acc_y;
		        Rec->getAcceptAngles(acc_x, acc_y);
		        acc_x *= 0.5;
		        acc_y *= 0.5;
		        if(Rec->getAcceptAngleType() == 0){ //Rectangular
			        if(! (fabs(theta_x) < acc_x && fabs(theta_y) < acc_y))
				        dels.push_back(j);
		        }
		        else{	//Elliptical
			        if( (theta_x*theta_x / (acc_x * acc_x) + theta_y*theta_y / (acc_y * acc_y)) > 1. )
				        dels.push_back(j);
		        }
	        }
            j++;
        }
        //Delete in reverse order
		int nd = dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}
    }

    //--------------------

	if(layout_method == 3 || refresh_only)
		//update the layout positions in the land object here since the post-process call isn't made after this
		SF.getLandObject()->setLayoutPositions(HelPos);

	//----For all of the heliostat positions, create the heliostat objects and assign the correct canting/focusing
	int Npos = HelPos.size();
	
    if(! SF.getSimInfoObject()->addSimulationNotice("Calculating heliostat canting and aim point information") ){
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation

	//Clear out the _heliostats array, we'll reconstruct it here
	Hvector *heliostats = SF.getHeliostats();
	heliostats->clear();
	heliostats->resize(Npos);
	
	//Set up the locations array
	vector<Heliostat> *helio_objects = SF.getHeliostatObjects();
	helio_objects->resize(Npos);
	Heliostat *hptr; //A temporary pointer to avoid retrieving with "at()" over and over
	int cant_method, focus_method;
	//A temporary point to pass to the template function
	Point P; P.x = 0; P.z = 0;
	Point Aim;		//The aim point [m]
	//Keep track of the min/max field extents too
	double xmin=9.e99, xmax=-9.e99, ymin=9.e99, ymax=-9.e99;
	double hpx, hpy, hpz;
	
	//For each heliostat position
	for(int i=0; i<Npos; i++){
		hpx = HelPos.at(i).x;
		hpy = HelPos.at(i).y;
		hpz = HelPos.at(i).z;
		//P.y = sqrt(pow(hpx, 2) + pow(hpy, 2));	//Determine the radial position. Set to y.
		Heliostat *htemp = SF.whichTemplate(SF.getTemplateRule(), HelPos.at(i));
		helio_objects->at(i) = *htemp;	//Copy the template to the heliostat
		hptr = &helio_objects->at(i);	//Save a pointer for future quick reference
		//Save a pointer to the template for future reference
		hptr->setMasterTemplate( htemp );

		//Set up the heliostat
		int layout_method = SF.getLayoutMethod();
		if(layout_method != 3){	
			//algorithmic layouts (not user defined)
			cant_method = hptr->getCantMethod();
			focus_method = hptr->getFocusMethod();
		}
		else{
			//User defined layouts - need to check for user defined canting and focusing
			if(layout->at(i).is_user_cant) {
				cant_method = Heliostat::CANT_TYPE::USER_VECTOR;
				hptr->IsUserCant( true );
			}
			else{
				cant_method = hptr->getCantMethod();
				hptr->IsUserCant( false );
			}

			if(layout->at(i).is_user_focus) {
				focus_method = 3;	//user defined
			}
			else{
				focus_method = hptr->getFocusMethod();
			}
		}

		hptr->setLocation(hpx, hpy, hpz);	//Set the position
		if(hpx < xmin){xmin = hpx;}
		if(hpx > xmax){xmax = hpx;}
		if(hpy < ymin){ymin = hpy;}
		if(hpy > ymax){ymax = hpy;}		//Track the min/max field extents

		//Quickly determine the aim point for initial calculations
		if(layout_method == 3 || refresh_only){
			//For user defined layouts...
			if(layout->at(i).is_user_aim){	//Check to see if the aim point is also supplied.
				SF.getFluxObject()->simpleAimPoint(*hptr, SF); //Calculate simple aim point first to associate heliostat with receiver.
				Aim = layout->at(i).aim;	//Assign the specified aim point
				hptr->setAimPoint(Aim);		
                //update the image plan aim point
                SF.getFluxObject()->keepExistingAimPoint(*hptr, SF, 0);
			}
			else{	
				SF.getFluxObject()->simpleAimPoint(*hptr, SF);
				Aim = *hptr->getAimPoint();				
			}
		}
		else{	//Otherwise,
			//Determine the simple aim point - doesn't account for flux limitations
			SF.getFluxObject()->simpleAimPoint(*hptr, SF);
			Aim = *hptr->getAimPoint();
		}
		
		//Aim points have been set
		SF.setAimpointStatus(true);

        //calculate and update the heliostat-to-tower vector
        Vect htow;
        htow.Set( Aim.x - hpx, Aim.y - hpy, Aim.z - hpz);
        
		//Calculate the slant range.. This should be the exact slant range.
		//double slant = sqrt(pow(Aim.x - hpx,2) + pow(Aim.y - hpy,2) + pow(Aim.z - hpz,2));
        double slant = Toolbox::vectmag( htow );

        //update the tower vector as a unit vector
        Toolbox::unitvect(htow);
        //Set the tower vector
        hptr->setTowerVector( htow );

		double crad;
		Reflector *cpanel;
		Point *pos;
		Vect vec;

		hptr->setSlantRange( slant );
		
        switch (cant_method)
        {
        case Heliostat::CANT_TYPE::FLAT:
			hptr->setCantRadius( 9.e99 );
            break;
        case Heliostat::CANT_TYPE::AT_SLANT:
			hptr->setCantRadius( slant );
            break;
        case Heliostat::CANT_TYPE::ON_AXIS_USER:
            //do nothing
            break;
        case Heliostat::CANT_TYPE::AT_DAY_HOUR:
            break;
        case Heliostat::CANT_TYPE::USER_VECTOR:
        {
                   //     if(layout_method == 3 || refresh_only)
       //     {
			    //crad = sqrt( pow(layout->at(i).cant.i, 2) + pow(layout->at(i).cant.j,2) + pow(layout->at(i).cant.k,2) );
			    //hptr->setCantRadius( crad );
			    //hptr->IsUserCant( true );
			    ////Calculate the cant vector for each panel
			    //for(unsigned int j=0; j<hptr->getPanels()->nrows(); j++){
				   // for(unsigned int m=0; m<hptr->getPanels()->ncols(); m++){
					  //  cpanel = &hptr->getPanels()->at(j,m);	//Point to the panel
					  //  pos = cpanel->getOrientation()->point();	//Get the panel position object
					  //  //un-normalized vector from the cant location to the aim point (relative to heliostat coordinates)
					  //  vec.Set( layout->at(i).cant.i - pos->x, layout->at(i).cant.j - pos->y, layout->at(i).cant.k - pos->z );	
					  //  //normalize the vector
					  //  Toolbox::unitvect( vec );
					  //  cpanel->setAim( vec );
				   // }
			    //}
       //     }
       //     else
            //{
                double crad = hptr->getCantVectScale();
                hptr->setCantRadius( crad );
                hptr->IsUserCant(false);

                //Calculate the cant vector for each panel
			    for(unsigned int j=0; j<hptr->getPanels()->nrows(); j++){
				    for(unsigned int m=0; m<hptr->getPanels()->ncols(); m++){
					    cpanel = &hptr->getPanels()->at(j,m);	//Point to the panel
					    pos = cpanel->getOrientation()->point();	//Get the panel position object
					    //un-normalized vector from the cant location to the aim point (relative to heliostat coordinates)
                        Vect* hcant = hptr->getCantVector();
					    vec.Set( hcant->i - pos->x, hcant->j - pos->y, hcant->k - pos->z );	
					    //normalize the vector
					    Toolbox::unitvect( vec );
					    cpanel->setAim( vec );
				    }
			    }
            //}
            break;
        }
        default:
            break;
        }


		//Choose how to focus the heliostat
		switch(focus_method)
		{
		case 0:	//Flat with no focusing
			hptr->setFocalLength( 1.e9 );
			break;
		case 1:	//Each at a focal length equal to their slant range
			hptr->setFocalLength( hptr->getSlantRange() );

			break;
		case 2:	//Average focal length in template group
			throw spexception("Average template focal length not currently implemented.");
			break;
		case 3:	//User defined focusing method

			break;
		}

        

		//Construct the panel(s) on the heliostat
        hptr->installPanels();

		//Assign a unique ID to the heliostat
		hptr->setId(i);

		//Update progress
		//_sim_info.setSimulationProgress(double(i)/double(Npos));
		if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation
	}
	
	//----Determine nearest neighbors---
	//First go through and put each heliostat into a regional group. The groups are in a cartesian mesh.
	//Save the heliostat field extents [xmax, xmin, ymax, ymin]
	
	SF.setHeliostatExtents(xmax, xmin, ymax, ymin);

	for(int i=0; i<Npos; i++){
		helio_objects->at(i).setInLayout(refresh_only);
	}
	
    if(! SF.getSimInfoObject()->addSimulationNotice("Determining nearest neighbors for each heliostat") ){
        SF.CancelSimulation();
        return false;
    }
	
	//Update the neighbor list based on zenith. Also initialize the layout groups.
	double *helio_extents = SF.getHeliostatExtents();
	bool isok = SF.UpdateNeighborList(helio_extents, 0. );	//_ambient.getSolarZenith());   don't include shadowing effects in layout (zenith = 0.)
	if(SF.CheckCancelStatus() || !isok) return false;	//check for cancelled simulation
	if(SF.isOpticalZoning() ){
        if(! SF.getSimInfoObject()->addSimulationNotice("Calculating layout optical groups") ){
            SF.CancelSimulation();
            return false;
        }
		isok = SF.UpdateLayoutGroups(helio_extents); //Assign heliostats to groups for simplified intercept factor calculations
		if(SF.CheckCancelStatus() || !isok) return false;
		SF.getSimInfoObject()->addSimulationNotice("Optical group calculations complete");
	}
	
	//----Over each heliostat, calculate optical efficiency for the set of design points
	
	//which simulation criteria should we use to filter the heliostats?
	int des_sim_detail = SF.getDesignSimDetail();
	unordered_map<int, Heliostat*> *helio_by_id = SF.getHeliostatsByID();

	if(des_sim_detail == LAYOUT_DETAIL::NO_FILTER || refresh_only){		//Do not filter any heliostats
		//Add all of the heliostat locations to the final vector
		heliostats->resize(Npos);
		for(int i=0; i<Npos; i++){	
			heliostats->at(i) = &helio_objects->at(i);
			heliostats->at(i)->setInLayout(true);	//All of the heliostats are included in the final layout
			//Update the tracking vector. This defines corner geometry for plotting.
			heliostats->at(i)->updateTrackVector(*ambient->getSunVector());
		}
		//Simulate the default design point to ensure equal comparison
		SF.SimulateTime(Ambient::getDefaultSimStep() );
		//Save the heliostats in an array by ID#
		helio_by_id->clear();
		for(int i=0; i<Npos; i++){
			(*helio_by_id)[ heliostats->at(i)->getId() ] = heliostats->at(i);
		}
	}
	else if(des_sim_detail != LAYOUT_DETAIL::NO_FILTER){	
					//1 :: Subset of days and hours, specified
					//2 :: Subset of days, all hours during the days
					//3 :: Full annual simulation from Weather file
					//4 :: Limited annual simulation from weather file
					//5 :: Representative profiles

		//Copy the previously calculated weather data into the object passed to this method
		SF.copySimulationStepData(wdata);

		//Check to see if at least one data value is available in the weather data object
		int nsim = wdata.size();
		if( nsim == 0 ){
			SF.getSimErrorObject()->addSimulationError((string)"No design-point data was provided for calculation of the solar field power output. Use setStep(...) to assign DNI, day, and hour info.",true,false);
			return false;
		}
		else{
			//if needed, determine clear sky DNI and replace the weather file DNI
			if (ambient->getInsolType() != Ambient::WEATHER ){

				DateTime *dtc = ambient->getDateTimeObj();
				DateTime dt;
				double dom, hour, month, dni, tdb, pres, wind, azzen[2], az, zen, step_weight;
				for(int i=0; i<wdata.size(); i++){
					//Get the design-point day, hour, and DNI
					wdata.getStep(i, dom, hour, month, dni, tdb, pres, wind, step_weight);

					//Convert the day of the month to a day of year
					int doy = ambient->getDateTimeObj()->GetDayOfYear(2011,int(month),int(dom));
				
					//Calculate the sun position
					ambient->setDateTime(hour, doy);
					//latitude, longitude, and elevation should be set in the input file
					ambient->calcSunPosition(azzen);
					az = azzen[0]; 
					zen = azzen[1];
					
					//calculate DNI
					double dniclr = ambient->calcInsolation(az, zen);
					//Set to the new value
					wdata.setStep(i, dom, hour, month, dniclr, tdb, pres, wind, step_weight);

				}

				//reset to the old date time
				ambient->setDateTime(dtc->_hour, dtc->GetDayOfYear());
			}
			
			//Set up the _heliostats array
			for(int i=0; i<Npos; i++){
				heliostats->at(i) = &helio_objects->at(i);		//At first, the heliostats array will contain all of the positions in the field
				heliostats->at(i)->resetMetrics();	//Reset all of the heliostat metrics to the original state 
			}
		}
		SF.getSimInfoObject()->setTotalSimulationCount(nsim);
			
		return true;	//Return a flag indicating that additional simulation is required
	}
	return false;	//if not a detailed design, we don't need additional simulation.

}

bool SolarField::DoLayout( SolarField *SF, sim_results *results, WeatherData *wdata, int sim_first, int sim_last){
	/* 
	This algorithm is static (i.e. it only refers to arguments passed to it and not to the
	"this" SolarField object. 

	The algorithm will simulate the performance of all heliostats in the SF heliostats array for timesteps
	in wdata from sim_first to sim_last. 
	
	If sim_first arg is not set, it will be set to 0.
	if sim_last arg is not set, it will be set to wdata->size()
	
	Threading note:
	This method is duplicated in the LayoutSimThread class. Ensure both methods are substantively consistent
	for predictable simulation behavior.

	*/
    if(! SF->getSimInfoObject()->addSimulationNotice("Simulating design-point conditions") ){
        SF->CancelSimulation();
        return false;
    }
		
	double dni, dom, doy, hour, month, tdb, pres, wind, step_weight;
	double az, zen, azzen[2];
			
	int Npos = SF->getHeliostats()->size();
				
	//Simulate for each time
	//int nsim_actual=0;	//keep track of the day of the year for _des_sim_detail = 3
	//double dni_ave=0.;	//keep track of the average DNI value

	if(SF->CheckCancelStatus()) return false;	//check for cancelled simulation

	if(sim_first < 0) sim_first = 0;
	if(sim_last < 0) sim_last = wdata->size();

	int nsim = sim_last - sim_first + 1;
	
	for(int i=sim_first; i<sim_last; i++){
		if(! SF->getSimInfoObject()->setCurrentSimulation(i+1) )
            return false;

		//Get the design-point day, hour, and DNI
		wdata->getStep(i, dom, hour, month, dni, tdb, pres, wind, step_weight);

		//Convert the day of the month to a day of year
		doy = SF->getAmbientObject()->getDateTimeObj()->GetDayOfYear(2011,int(month),int(dom));
				

		//Calculate the sun position
		SF->getAmbientObject()->setDateTime(hour, doy);
		//latitude, longitude, and elevation should be set in the input file
		SF->getAmbientObject()->calcSunPosition(azzen);
		az = azzen[0]; 
		zen = azzen[1];
		//If the sun is not above the horizon, don't continue
		if( zen > SF->pi * 0.5 )
				continue;
		
				
		//Simulate field performance
		double args[] = {dni, tdb, wind, pres/1000., step_weight};
		SF->Simulate(args, 5, true);
		//nsim_actual ++; dni_ave+=dni;

		//store the results
		results->push_back( sim_result() );
		results->back().process_analytical_simulation( *SF, 0); //2);

		if(SF->CheckCancelStatus()) return false;	//check for cancelled simulation				
	}

    return true;
}		

void SolarField::ProcessLayoutResults( sim_results *results, int nsim_total){
	/*
	The sort metrics are mapped from the GUI in the following order:
	0	|	Power to the receiver
	1	|	Total efficiency
	2	|	Cosine efficiency
	3	|	Attenuation efficiency
	4	|	Intercept efficiency
	5	|	Blocking efficiency
	6	|	Shadowing efficiency
	7	|	Market-weighted power to the receiver

	Default is 7.
	*/

	_sim_info.ResetValues();
    if(! _sim_info.addSimulationNotice("Ranking and filtering heliostats") ) {
        CancelSimulation();
        return;
    }

	int Npos = _heliostats.size();
	//Update each heliostat with the ranking metric and sort the heliostats by the value 
	vector<double> hsort(Npos);
	double rmet; //, nsimd=(float)nsim_total;
	
	//Save the heliostats in an array by ID#
	_helio_by_id.clear();
	for(int i=0; i<Npos; i++){
		_helio_by_id[ _heliostats.at(i)->getId() ] = _heliostats.at(i);
	}

	//Of all of the results provided, calculate the average of the ranking metric
	int rid = getHelioSortMethod();
	
	int nresults = (int)results->size();

    //compile the results from each simulation by heliostat
    for(int i=0; i<Npos; i++){
		int hid = _heliostats.at(i)->getId();	//use the heliostat ID from the first result to collect all of the other results
		rmet = 0.;      //ranking metric
		for(int j=0; j<nresults; j++)
            rmet += results->at(j).data_by_helio[ hid ].getDataByIndex( rid );      //accumulate ranking metric as specified in rid

        //normalize for available heliostat power if applicable
        double afact = 1.;
        if( rid == helio_perf_data::PERF_VALUES::POWER_TO_REC || rid == helio_perf_data::PERF_VALUES::POWER_VALUE )
            afact = _heliostats.at(i)->getArea();
        
        double rank_val = rmet / (afact*(float)nresults);       //Calculate the normalized ranking metric. Divide by heliostat area if needed
		
        _helio_by_id[hid]->setRankingMetricValue( rank_val );   //store the value
		hsort.at(i) = rank_val;                                 //also store in the sort array
		
	}

    //quicksort by ranking metric value. Correlate _heliostats vector
	quicksort(hsort, _heliostats, 0, Npos-1);

    //Simulate the default design point to ensure equal comparison
	double az_des, zen_des;
    if(! CalcDesignPtSunPosition(_sun_loc_des, az_des, zen_des) )
        return;
	double args[] = {_dni_des, 25., 1., 0., 1.};;
	if(! SimulateTime(90.-zen_des, az_des, args, 5) ){
        throw spexception("The design point sun position is invalid. Simulation could not be completed.");
        return;
    }
	

	_q_to_rec = 0.;
	for(int i=0; i<Npos; i++){
		_q_to_rec += _heliostats.at(i)->getPowerToReceiver();
	}

	//Calculate the required incident power before thermal losses
	double q_loss_tot = 0.;
	for(int i=0; i<(int)_receivers.size(); i++){
		_receivers.at(i)->CalculateThermalLoss(1., 0.);
		double 
			ql = _receivers.at(i)->getReceiverThermalLoss(),
			qp = _receivers.at(i)->getReceiverPipingLoss();
		q_loss_tot += ql + qp;
	}

	double q_inc_des = _q_des + q_loss_tot;
	_q_des_withloss = q_inc_des; //save this

	//Check that the total power available is at least as much as the design point requirement. If not, notify
	//the user that their specified design power is too high for the layout parameters.
	if(_q_to_rec < q_inc_des*1.e6){
		string units;
		double 
			xexp = log10(_q_to_rec),
			xmult;
		if(xexp>9.) {
			units = "GW";
			xmult = 0.001;
		}
		else if(xexp>6.) {
			units = "MW";
			xmult = 1.;
		}
		else{
			units = "kW";
			xmult = 1000.;
		}
			
		char msg[1000];
		sprintf(msg, 
			"The maximum available power for this field layout is %.2f %s, and the required design power is %.2f %s."
			"The field cannot generate sufficient power to meet the design requirement. Consider adjusting design point conditions "
			"to generate a satisfactory design.", (float)(_q_to_rec*1.e-6*xmult), units.c_str(), (float)(q_inc_des*xmult), units.c_str());
		_q_des_withloss = q_inc_des;
		_sim_error.addSimulationError(string(msg)); //, true, true);
		//return;
	}

	double filter_frac = _is_prox_filter ? _prox_filter_frac : 0.;
	
	//Determine the heliostats that will be used for the plant
	double q_cutoff=_q_to_rec;	//[W] countdown variable to decide which heliostats to include
	int isave;
	int nfilter=0;
	double q_replace=0.;
	for(isave=0; isave<Npos; isave++){
		//
		double q = _heliostats.at(isave)->getPowerToReceiver();
		q_cutoff += -q;
		if(q_cutoff < q_inc_des*1.e6) {
			nfilter++;
			q_replace += q;
		}
		if(q_cutoff < q_inc_des*1.e6*(1.-filter_frac)){break;}
	}
	//The value of isave is 1 entry more than satisfied the criteria.

	if(_is_prox_filter){
		//sort the last nfilter*2 heliostats by proximity to the reciever and use the closest ones. 
		Hvector hfilter;
		vector<double> prox;
		for(int i=max(isave-2*(nfilter-1), 0); i<isave; i++){
			hfilter.push_back( _heliostats.at(i) );
			prox.push_back( hfilter.back()->getRadialPos() );
		}
		quicksort(prox, hfilter, 0, (int)prox.size()-1);
		//swap out the heliostats in the main vector with the closer ones.
		double q_replace_new=0.;
		int ireplace = isave-1;
		int nhf = (int)hfilter.size();
		int irct=0;
		while(q_replace_new < q_replace && irct < nhf){
			_heliostats.at(ireplace) = hfilter.at(irct);
			q_replace_new += _heliostats.at(ireplace)->getPowerToReceiver();
			ireplace--;
			irct++;
		}
		//update the isave value
		isave = ireplace+1;
	}

	//Delete all of the entries up to isave-1.
	_heliostats.erase(_heliostats.begin(), _heliostats.begin()+isave);

    //Remove any heliostat in the layout that does not deliver any power to the receiver
    isave = 0;
    for(size_t i=0; i<_heliostats.size(); i++){
        if(_heliostats.at(i)->getPowerToReceiver() > 0.) {
            isave = i;
            break;
        }
    }
    if(isave > 0)
        _heliostats.erase(_heliostats.begin(), _heliostats.begin()+isave);
	Npos = _heliostats.size();

	//Save the heliostats in an array by ID#
	_helio_by_id.clear();
	for(int i=0; i<Npos; i++){
		_helio_by_id[ _heliostats.at(i)->getId() ] = _heliostats.at(i);
	}

	//Mark these heliostats as contained in the final layout and recalculate the field extents
	_helio_extents[0] = -9e9; //xmax
	_helio_extents[1] = 9e9;	//xmin
	_helio_extents[2] = -9e9;	//ymax
	_helio_extents[3] = 9e9;	//ymin
	for(int i=0; i<Npos; i++){ 
		_heliostats.at(i)->setInLayout(true);	//Set as in the layout
		//Refactor the extents
		Point *loc = _heliostats.at(i)->getLocation();
		if(loc->x > _helio_extents[0]) _helio_extents[0] = loc->x;
		if(loc->x < _helio_extents[1]) _helio_extents[1] = loc->x;
		if(loc->y > _helio_extents[2]) _helio_extents[2] = loc->y;
		if(loc->y < _helio_extents[3]) _helio_extents[3] = loc->y;
	}
	//Limit the extents to always include the plot origin
	if(_helio_extents[0] < 0.) _helio_extents[0] = 0.;
	if(_helio_extents[1] > 0.) _helio_extents[1] = 0.;
	if(_helio_extents[2] < 0.) _helio_extents[2] = 0.;
	if(_helio_extents[3] > 0.) _helio_extents[3] = 0.;
	
	//Create an estimate of the annual energy output based on the filtered heliostat list
	_estimated_annual_power = 0.;

	for(int i=0; i<Npos; i++){
        int hid = _heliostats.at(i)->getId();
	    for(int j=0; j<nresults; j++){
			_estimated_annual_power += results->at(j).data_by_helio[ hid ].getDataByIndex( helio_perf_data::PERF_VALUES::POWER_VALUE );  //this include receiver efficiency penalty
		}
	}

	//update the layout positions in the land area calculation
	vector<Point> lpos(_heliostats.size());
	for(int i=0; i<(int)_heliostats.size(); i++)
		lpos.at(i) = *_heliostats.at(i)->getLocation();
	_land.setLayoutPositions(lpos);
	

	return;
}

void SolarField::AnnualEfficiencySimulation( var_set &vset, SolarField &SF, sim_results &results){ 
	string wf = vset["ambient"][0]["weather_file"].value;
	
	SolarField::AnnualEfficiencySimulation( wf, &SF, results); 
}

void SolarField::AnnualEfficiencySimulation( string weather_file, SolarField *SF, sim_results &results) //, double *azs, double *zens, double *met)	//overload
{
	/* 
	Take the simulations provided in the "results" array and create an annual simulation from the
	specified weather file. Each heliostat is evaluated based on the efficiency values provided
	in the results along with the associated solar position.
	*/
	double pi, Pi = pi = acos(-1.);
	//Create arrays of the available sun positions
	int nresult = (int)results.size();
	vector<double>
		solaz(nresult),
		solzen(nresult);
	for(int i=0; i<nresult; i++){
		solaz.at(i) = results.at(i).solar_az;
		solzen.at(i) = results.at(i).solar_zen;
	}

	Ambient *amb = SF->getAmbientObject();

	WeatherData wdannual;
	Ambient::readWeatherFile(wdannual, weather_file, amb);
	
	
	int rindex = SF->getHelioSortMethod();
	int Npos = SF->getHeliostats()->size();
	//Clear the existing ranking metric value from the heliostats
	Hvector *helios = SF->getHeliostats();
	for(int i=0; i<Npos; i++){
		helios->at(i)->setRankingMetricValue(0.);
	}

	//If the ranking requires TOD factors, get the array now
	matrix_t<int> *TOD;
	if(rindex == helio_perf_data::PERF_VALUES::POWER_VALUE)
		TOD = SF->getFinancialObject()->getHourlyTODSchedule();
	double *pfs = SF->getFinancialObject()->getPaymentFactors();
	bool is_pmt_factors = SF->getFinancialObject()->isPaymentFactors();
	//Get design point DNI for some simulations
	double dni_des = SF->getDesignPointDNI();

	unordered_map<int, helio_perf_data> *resmap[3];

	//Simulate each step
	int nsim = (int)wdannual.size();
	double nsimd = 1./(double)nsim;
	unordered_map<int, double> rank_temp;
	//initialize temp rank value array
	for(int j=0; j<Npos; j++)
		rank_temp[helios->at(j)->getId()] = 0.;

	simulation_info *siminfo = SF->getSimInfoObject();
	siminfo->setTotalSimulationCount(nsim);
	siminfo->setCurrentSimulation(0);
	
    if(! siminfo->addSimulationNotice("Simulating hourly weather profile...") ){
        SF->CancelSimulation();
        return;
    }
	
	for(int i=0; i<nsim; i++){

		//Calculate the solar position based on the time step
		//Convert the day of the month to a day of year
		int month = (int)wdannual.Month.at(i);
		int mday = (int)wdannual.Day.at(i);
		int doy = DateTime::CalculateDayOfYear(2011, month, mday);
		double hour = wdannual.Hour.at(i)+0.5;
		//Calculate the sun position
		amb->setDateTime(hour, (double)doy);
		//latitude, longitude, and elevation should be set in the input file
		double azzen[2], az, zen;
		amb->calcSunPosition(azzen);
		az = azzen[0]; 
		zen = azzen[1];

		double dni = wdannual.DNI.at(i);
				
		//If the sun is not above the horizon, don't process
		if( zen > Pi*0.5) continue;
		
		//Find the 3 efficiency values closest to the current sun position
		int jsave[3]={0,0,0};
		double rdiff[3] = {9.e9, 9.e9, 9.e9};
		for(int j=0; j<nresult; j++){
			double rdiff2 = sqrt( std::pow(az-solaz.at(j),2) + std::pow(zen-solzen.at(j),2) );
			int k=3; 
			while( rdiff2 < rdiff[k-1] && k>0) k+= -1;	//rdiff is in order of closest to farthest. find where the new point should go
			//If a position is found within the array...
			if(k<3){
				//shift values in the arrays
				for(int kk=2; kk>k; kk+= -1){
					if(kk>0){
						rdiff[kk] = rdiff[kk-1];
						jsave[kk] = jsave[kk-1];
					}
				}
				//insert new value
				rdiff[k] = rdiff2;
				jsave[k] = j;
			}
		}
		
		//Calculate the XYZ plane based on cross-product of the two vectors. Normal vector components are multipliers on plane fit.
		Vect t1, t2, cp;
		for(int j=0; j<3; j++)
			resmap[j] = &results.at(jsave[j]).data_by_helio;
		double rm_az[3], rm_zen[3];
		for(int j=0; j<3; j++){
			rm_az[j] = results.at(jsave[j]).solar_az;
			rm_zen[j] = results.at(jsave[j]).solar_zen;
		}
		
		for(unordered_map<int, helio_perf_data>::iterator it = resmap[0]->begin(); it != resmap[0]->end(); it++){ //For each heliostat
			int hid = it->first;
			double zdat[3];
			for(int j=0; j<3; j++)
				zdat[j] = (*resmap[j])[hid].getDataByIndex( rindex );

			t1.Set( rm_az[1] - rm_az[0], rm_zen[1] - rm_zen[0], zdat[1] - zdat[0]); 
			t2.Set( rm_az[2] - rm_az[0], rm_zen[2] - rm_zen[0], zdat[2] - zdat[0]);
			cp = Toolbox::crossprod(t1, t2);

			if(cp.k < 0.){
				cp.i = - cp.i;
				cp.j = - cp.j; 
				cp.k = - cp.k;
			}

			//find min and max
			double zmin = 9.e9, zmax = -9.e9;
			for(int j=0; j<3; j++){
				if(zdat[j] < zmin) zmin = zdat[j];
				if(zdat[j] > zmax) zmax = zdat[j];
			}
						
			//Now based on the location of the point in az,el coordinates, we can calculate an interpolated efficiency
			double z_interp = zdat[0] - (cp.i*(az-rm_az[0]) + cp.j*(zen-rm_zen[0]))/cp.k;

			if(z_interp < zmin) z_interp = zmin;
			if(z_interp > zmax) z_interp = zmax;


			//Calculate the ranking metric.
			double payfactor = 1., zval ;
			switch(rindex)
			{
			case helio_perf_data::PERF_VALUES::POWER_VALUE:
				if(is_pmt_factors)
					payfactor = pfs[TOD->at(i)-1];
			case helio_perf_data::PERF_VALUES::POWER_TO_REC:
				zval = dni/dni_des * payfactor * z_interp * nsimd;
				break;
			default:
				zval = z_interp * nsimd;
			}
			rank_temp[hid] += zval;
			
		}
		
		if(i%200==0){
            if(! siminfo->setCurrentSimulation(i) ) break;
        }
	}
	siminfo->setCurrentSimulation(0);
	//Clear the results array and set up a dummy result that can be used to sort the field
	if(nsim > 1)
		results.erase( results.begin() + 1, results.end());
	for(unordered_map<int,double>::iterator it = rank_temp.begin(); it != rank_temp.end(); it++)
		results.begin()->data_by_helio[it->first].setDataByIndex( rindex, it->second );
		

}

Heliostat *SolarField::whichTemplate(int method, Point &pos){
	/*
	This function takes as arguments an integer indicating the method for determining which heliostat 
	template to use (method) and the current point {x,y,z} location of the heliostat. The method
	uses information attributes of the current SolarField object to determine which heliostat template
	is appropriate.

	--Methods description--
	0 = Use single template
	1 = Specified range
	2 = Even radial distribution	

		The heliostat field is broken into equidistant sections depending on the number
		of heliostat templates provided. Heliostats are assigned to templates depending
		on which section they fall into. 

	*/

	int Nht = _helio_templates.size();
    //count the number of enabled templates
    int Nht_active = 0;
    for(int i=0; i<(int)_helio_templates.size(); i++)
        if( _helio_templates.at(i)->IsEnabled() )
            Nht_active++;

	//get the field limits from the land class
	double rad[2];
	
	double 
		rpos = sqrt(pow(pos.x,2) + pow(pos.y,2))/_tht, 
		azpos = atan2(pos.x, pos.y);

	_land.getExtents(rad);
	double
		radmin = rad[0],
		radmax = rad[1];
	
	switch(method)
    {
    case SolarField::TEMPLATE_RULE::SINGLE:
        //Use single template
        return _helio_templates.find( _temp_which )->second;
        /*for(int i=0; i<(int)_helio_templates.size(); i++)
        {
            if( _temp_which == _helio_templates.at(i)->getId() )
                return _helio_templates.at(i);
        }*/
    case SolarField::TEMPLATE_RULE::SPEC_RANGE:
 	{
		double tradmax, tradmin, tazmax, tazmin;
		for(int i=0; i<Nht; i++){
			_helio_templates.at(i)->getTemplateRange(tradmin, tradmax, tazmin, tazmax);
			if(rpos >= tradmin && rpos < tradmax && azpos >= tazmin && azpos < tazmax)
				return _helio_templates.at(i);
		}
		//none caught.. return the first template
		return _helio_templates.at(0);
		break;
	}
    case SolarField::TEMPLATE_RULE::EVEN_DIST:
	{
        //calculate which template is being used by comparing the current position to the radial range of each template.
		int ht = int(floor((rpos - radmin)/( (radmax+0.0001-radmin)/double(Nht_active) )));
        int ht_ct = -1;     //Counter for number of enabled templates
        int ht_save = 0;    //Save the value of the applicable template
        for(int i=0; i<Nht; i++)
        {
            if( _helio_templates.at(i)->IsEnabled() )
                ht_ct++;    //only count enabled templates
            if( ht_ct == ht )
            {               //once the template count matches the calculated number, save and move on
                ht_save = i;
                break;
            }
        }

		return _helio_templates.at(ht_save);
	}
    default:
        break;
    }

    throw spexception("An error occurred while calculating heliostat template placement. Please contact support for debugging help.");

}

void SolarField::TemplateRange(int pos_order, int method, double *rrange, double *arange){
	/* 
	This function is the inverse of 'SolarField::whichTemplate'. Provide a heliostat template
	index within the larger set and a method for dividing the field, and this calculates the 
	valid range of the template.

	>> pos_order = the position of this template in the field. Lower is nearer the receiver.

	<< rrange[2] . Sets the values of len=2 array : {rmin,rmax}
	
	--Methods description--
	0 = Use single template
	1 = Specified range
	2 = Even radial distribution	
	*/

	int Nht = _helio_templates.size();
	//get the field limits from the land class
	double rad[2];
	_land.getExtents(rad, _tht);
	double
		radmin = rad[0],
		radmax = rad[1];
	double drad;
	double tradmin, tradmax, tazmin, tazmax;
	switch(method)
	{
	case 0:	//Use single template
		rrange[0] = radmin;
		rrange[1] = radmax;
		arange[0] = -pi;
		arange[1] = pi;
		return;
		break;
	case 1:	//Specified range

		_helio_templates.at(pos_order)->getTemplateRange(tradmin, tradmax, tazmin, tazmax);
		rrange[0] = tradmin;
		rrange[1] = tradmax;
		arange[0] = tazmin;
		arange[1] = tazmax;
		return;
		break;

	case 2:	//equal spatial separation
		drad = (radmax - radmin)/float(Nht);
		rrange[0] = radmin + pos_order*drad;
		rrange[1] = rrange[0] + drad;
		arange[0] = -pi;
		arange[1] = pi;
		return;
		break;
	default:
		rrange[0] = radmin;
		rrange[1] = radmax;
		arange[0] = -pi;
		arange[1] = pi;
		
	}


}

void SolarField::radialStaggerPositions(vector<Point> &HelPos)
{
	/*
	Calculate the possible heliostat positions in the solar field, given certain minimum/maximum extent
	requirements, heliostat geometry, and tower height. 

	This function requires that the following SolarField attributes be defined:
	-> radmin
	-> radmax
	-> q_des
	-> tht
	-> helio_templates
	-> land (if land filter is used)
	-> spacing_reset
	
	Note that DELSOL uses a variable called "FSLIP" to remove heliostats from the first row when spacing
	is reset. This is based on an empirical relationship that defines the average spacing of a radial "zone"
	and moves inward until heliostat must be removed. This convention is not adopted here. Instead, the
	spacing is determined either by optimization or a user-specified initial spacing factor, and spacing 
	is reset to the initial ratio when the current row azimuth spacing divided by the compressed row spacing 
	exceeds the value "_spacing_reset".

	Radial spacing can be calculated as follows:
	-> Using an empirical relationship from DELSOL
	-> To eliminate blocking but not shading
	-> Optimized? (not yet implemented)

	The heliostat templates will be used according to the following rules:
	-> For 'N' templates in a 1-D array, the field will be broken into N radial sections from radmin to radmax.
	   Each template will be used in its corresponding radial section, with the first template corresponding
	   to the first radial group.
	*/

	//any declarations
	int Nht; //heliostat templates in the radial directions
	int i,j;
	int N_max;   //Upper estimate for the number of heliostats in the field, sizes the arrays

	//Calculate limits in meters
	//get the field limits from the land class
	double rad[2];
	_land.getExtents(rad, _tht);
	double
		radmint = rad[0],
		radmaxt = rad[1];
	
	//First determine how to split up the templates
	Nht = _helio_templates.size();	//Number in the radial direction

	//Calculate an upper estimate of the size of the heliostat positions array to avoid resizing all the time
    {   //ensure local scope for these temporary variables
	    double r_coll_temp;
	    double r_coll_min = 9.e9; 
	    for(htemp_map::iterator it=_helio_templates.begin(); it != _helio_templates.end(); it++){
		    r_coll_temp = it->second->getCollisionRadius();
		    if(r_coll_temp < r_coll_min) r_coll_min = r_coll_temp; //minimum collision radius in any combination
	    }
	    int nr_max = int((radmaxt - radmint)/(r_coll_min*2.)); 
	    int naz_max = int((radmaxt + radmint)/2.*(_accept_max - _accept_min)/(r_coll_min*2.));
	    N_max = nr_max * naz_max;  //Estimate the array size
    }

	HelPos.reserve(N_max);

    //choose which (initial) template to use to lay out the positions
    Heliostat *Htemp=0;
    switch (_template_rule)
    {
    case SolarField::TEMPLATE_RULE::SINGLE:
        Htemp = _helio_templates.find( _temp_which )->second;
        break;
    case SolarField::TEMPLATE_RULE::SPEC_RANGE:
    case SolarField::TEMPLATE_RULE::EVEN_DIST:
        // Use the first enabled template in the list
        for(int i=0; i<(int)_helio_templates.size(); i++)
        {
            if( _helio_templates.at(i)->IsEnabled() )
            {
                Htemp = _helio_templates.at(i);
                break;
            }
        }
        break;
    default:
        throw spexception("An invalid heliostat template rule was specified. Please contact support for debugging help.");
    }

	//how to calculate radial spacing of the rows?
	if(_rad_spacing_method == HELIO_SPACING_METHOD::DELSOL_EMPIRICAL)	//use the empirical relationship from delsol
    {
		/* 
		Documentation on these methods is from Kistler (1986), pages 39-41.
		DELSOL3 code lines 1223-1286.

		There are separate relationships depending on surround/cavity receiver, round/rectangular heliostats.

		For this method, only 1 heliostat template can be used.
		*/

		//Check to see if only 1 heliostat template is used.


		/*----------Calculate the row positions----------*/

		int nr = 1; //row counter
		double r_c = radmint; //current row position
		bool is_slip = true;    //initialize

		//Calculate the pointing angle for the first row
		double phi_0 = atan(_tht/r_c);	//Elevation angle from the heliostats in the first row to the receiver
		double tan_phi_0 = _tht/r_c;
		double r_reset = r_c; //Hold on to the radius where the spacing has reset

        //Calculate azimuthal spacing variables
		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz, dr_c;
        int Nhelio = 0;
		
		azmid = (_accept_max + _accept_min)/2.;	//[rad] The midpoint of the acceptance window
        int hpr=-1;  //heliostats per row are calculated after slip planes

		while(r_c < radmaxt){
			/* 
			Calculations in this loop use the current radial position value (r_c), the
			flag for whether the row is a slip plane (is_slip), and the elevation angle 
			phi_0 that is consistent with r_c. These should be initialized and are updated 
			at the end of the loop.
			*/

            //Choose the heliostat template based on the current row position, updating only after slip planes
            if(is_slip)
            {
                Point cpos;
                cpos.Set( 0., r_c, 0. );
                Htemp = whichTemplate(_template_rule, cpos );
            }

            bool is_round = Htemp->IsRound();

            double H2; //heliostat half-height
		    if(is_round) {
			    //round
			    H2 = Htemp->getWidth()/2.;	//equals diameter/2
		    }
		    else{
			    //Rectangular
			    H2 = Htemp->getHeight()/2.;			
		    }

		    //Get the minimum separation between heliostats that ensures no collision
		    double r_coll = Htemp->getCollisionRadius();    //Collision radius for current template
		    double hw = Htemp->getWidth();

			//The radial separation formula is the same for both round and rectangular heliostats
			double rsep = 1.1442399/tan_phi_0-1.093519+3.0683558*phi_0-1.1255617*pow(phi_0,2);
			double asep;

			//Calculate azimuthal separation
			if(is_round){
				//Round
				asep = 1.609666+0.29654848*phi_0+0.019137019/(phi_0-0.012341664);
				asep *= 1./(1.-rsep*hw/(_tht*2. * r_c));	//D1275 is always used.
				rsep *= .5;	//Correct for the half-size convention used
				dr_c = rsep*Htemp->getWidth();
			}
			else{
				//Rectangular
				asep = (1.7490871+0.63964099*phi_0+0.028726279/(phi_0-0.049023315));
				asep *= 1./(1.-rsep*hw/(_tht*2. * r_c));	//D1275 is always used.
				rsep *= .5;	//Correct for the half-size convention used
				dr_c = rsep*Htemp->getHeight();
			}
			
			//----Now add heliostats to each row----
			//If the collision radius limit is exceeded, remove heliostats. 
			//daz_init = Azimuthal straight-line distance
			daz_init = hw*asep;	//Normal spacing
			int col_factor = r_coll*2. > hw*asep ? 2 : 1; 
			
			//Should this row reset the spacing? 
			if( is_slip )
            {
				az_ang = 2.*atan2(daz_init, 2.*r_c);	//The angular spacing of the heliostats in the row
				
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_accept_max - _accept_min), 2.*pi)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}

            if( hpr < 0 )
                throw spexception("An algorithmic error occurred during heliostat placement. Please contact support for debugging help.");

			j=0;
			while(j<hpr){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(nr%2);	//heliostat azimuth angle
				HelPos.push_back(Point());
				HelPos.at(Nhelio).x = r_c*sin(haz);
				HelPos.at(Nhelio).y = r_c*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
				j += col_factor;	//If we're skipping heliostats, col_factor will increment by 2
			}

			//Increment to the next row, but make sure there's enough room to avoid collision
			r_c += max(dr_c, r_coll*2.);

			//Is the next row a slip plane?
			if(r_c/r_reset > _spacing_reset){
				is_slip = true;
				r_reset = r_c;
			}
			else{
				is_slip = false;
			}

			nr++;	//Row number, starts at 1
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_tht/r_c);
			tan_phi_0 = _tht/r_c;
		}


	}
	else if( (_rad_spacing_method == HELIO_SPACING_METHOD::NO_BLOCKING || _rad_spacing_method == HELIO_SPACING_METHOD::NO_BLOCK_DENSE)
            && Htemp->IsRound()){
		/* 
		Space using radial stagger with the row position chosen for "close packing"
		from the perspective of the receiver.
		*/

		int nr=1;	//row counter
		double r_c = radmint; //Initialize
		bool is_slip = true;


		//For round heliostats, all heliostats must be round (no multiple templates. Macro-level geometry
		//will use the first heliostat (excludes specific canting, aiming, focusing etc.).

        double phi_0 = atan(_tht/r_c);	//elevation angle of the heliostats in the first row

		double r_reset = r_c;	//Hold on to the radius where the spacing has been reset
		

		
		//Calculate azimuthal spacing variables
		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz, dr_c;
		int Nhelio = 0;
		
		azmid = (_accept_max + _accept_min)/2.;	//[rad] The midpoint of the acceptance window

		//Keep track of the row positions
		vector<double> rowpos; 
        rowpos.push_back(r_c);
		//keep track of whether each row was a slip plane
		vector<bool> slips; 
        slips.push_back(true);


		while(r_c < radmaxt){

            //Choose the heliostat template based on the current row position, updating only after slip planes
            if(is_slip)
            {
                Point cpos;
                cpos.Set( 0., r_c, 0. );
                Htemp = whichTemplate(_template_rule, cpos );
            }

            double Hd = Htemp->getWidth();	//Heliostat diameter
		    double Hrad = Hd/2.;

			//----Add heliostats to this row----
			
			//Should this row reset the spacing? 
            int hpr=-1;  //heliostats per row are calculated after slip planes
			if( is_slip ){
				//daz_init = Azimuthal straight-line distance
				daz_init = Hd*max(1.,_az_spacing);	//Normal spacing
				
				az_ang = 2.*asin(daz_init*.5/r_c);	//The angular spacing of the heliostats in the row
				
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_accept_max - _accept_min), 2.*pi)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}
            if( hpr < 0 )
                throw spexception("An algorithmic error occurred during heliostat placement. Please contact support for debugging help.");

			j=0;
			while(j<hpr){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(nr%2);	//heliostat azimuth angle
				HelPos.push_back(Point());
				HelPos.at(Nhelio).x = r_c*sin(haz);
				HelPos.at(Nhelio).y = r_c*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
				j ++;	//If we're skipping heliostats, col_factor will increment by 2
			}

			//--- Calculate the position of the next row ---
			dr_c = sqrt( pow(Hd,2) - pow(r_c*sin(az_ang/2.),2) )/sin(phi_0);


			//Increment to the next row
			r_c += dr_c;

			//Calculate the spacing between the current row and the previous 2 rows that
			//would result in blocking
			is_slip = false;
			if(nr>2){
				if(! slips.at(nr-2)){
					//If the last row was a slip plane, don't do these calculations
					double drlim = Hd/sin(phi_0);
					if( (r_c - rowpos.at(nr-3)) < drlim || dr_c < Hd/2.){
						is_slip = true;
						r_c += -dr_c + max(drlim, Hd);
						r_reset = r_c;
					}
				}
			}

			nr++;	//Row number, starts at 1
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_tht/r_c);
			
			slips.push_back(is_slip);
			rowpos.push_back(r_c);
		}

	}
	else if( (_rad_spacing_method == HELIO_SPACING_METHOD::NO_BLOCKING || _rad_spacing_method == HELIO_SPACING_METHOD::NO_BLOCK_DENSE)
            && !Htemp->IsRound()){	//Space to eliminate blocking - rectangular heliostats
		//***calculate the row positions***
		
		int nr=1;	//row counter
		double r_0 = radmint; 
        double r_c = r_0;	//initialize
		
        vector<double> rowpos; 
        vector<bool> slips;
        vector<bool> is_compact;

		rowpos.push_back(r_0);	//add the first row
		slips.push_back(true);
		
		//Calculate the minimum row separation distance, depends on default azimuthal spacing
		//..The azimuthal separation factor - the ratio of the actual separation azimuthally to the collision radius
		double H2 = Htemp->getHeight()/2.;  //Heliostat half-height
		
        

		//calculate pointing angle for the first row
		double phi_0 = atan(_tht/r_0);	//elevation angle of the heliostats in the first row
		
		double r_reset = r_c;	//Hold on to the radius where the spacing has reset
	    
        Point hloc;
		while(r_c < radmaxt){
			
            //get the new template
            hloc.Set(r_0,0.,0.);	//Radial position (y-component doesn't matter with this layout in terms of the template to use)
            Htemp = whichTemplate(_template_rule, hloc );
            H2 = Htemp->getHeight()/2.;  //Heliostat half-height
            double W2 = Htemp->getWidth()/2.;   //heliostat half-width

            double r_coll = Htemp->getCollisionRadius();
            double fr = H2*2.*_az_spacing/r_coll;
		    //double dr_min = 2.*sqrt( pow(2.*r_coll, 2) - pow(r_coll * fr/2.,2) );
            double dr_min = 2. * sqrt( 4 * r_coll * r_coll - pow(_az_spacing*W2, 2) );

			//from pointing angle, calculate other needed info
			double z_0u = cos(phi_0)*H2;	//height of the upper heliostat edge
			double r_0u = r_0 + sin(phi_0)*H2;	//Radial position of the upper heliostat edge

            //Calculate the next row position based on similar triangles between tower and upper/lower corners of adjacent heliostats.
            r_c = r_0u * (_tht + z_0u) / (_tht - z_0u) + sin(phi_0)*H2;

            //Is this row in the inner compact region?
            bool row_compact = r_c - r_0 < 2.* r_coll *_trans_limit_fact;
            if( _rad_spacing_method != HELIO_SPACING_METHOD::NO_BLOCK_DENSE )
                row_compact = false;    //only allow compact layout if requested

            //Has the row compact flag just changed?
            bool row_compact_switch = false;
            
            //retroactively handle the first row
            if( is_compact.empty() ){
                if( row_compact )
                {
                    //The next row is compact, therefore the first must also be
                    is_compact.push_back( true );
                }
                else{
                    is_compact.push_back( false );
                }
            }
            else{
                if( is_compact.back() && !row_compact )
                    row_compact_switch = true;
            }
			
            //Increment to the next row, but make sure there's enough room to avoid collision
			r_c = r_0 + max(r_c - r_0, dr_min);
            
            bool is_slip = (r_c+r_0)/2./r_reset > _spacing_reset || row_compact_switch;

			//check whether the next row is outside the max bounds
			if(r_c > radmaxt) break;

            //manage the next two rows. Call the template check for each subsequent row to ensure
            //the correct template is used for each position.
            Point hpos;
            hpos.Set( (r_c + r_0)/2., 0., 0. );
            Heliostat *Htemp_r1 = whichTemplate( _template_rule, hpos );
            hpos.Set( r_c, 0., 0. );
            Heliostat *Htemp_r2 = whichTemplate( _template_rule, hpos );

            //first handle the case where the intermediate row uses a different template
            if( Htemp != Htemp_r1 )
            {
                //the intermediate row causes a template switch. min spacing enforced
                nr++;
                H2 = Htemp_r1->getHeight()/2.;  //Heliostat half-height

                //update the calculations
                r_coll = Htemp_r1->getCollisionRadius();
                fr = H2*2.*_az_spacing/r_coll;
			    
                dr_min = sqrt( pow(2.*r_coll, 2) - pow(r_coll * fr/2.,2) ); //update calculation
                //r_c = r_0 + max( (r_c - r_0)/2., dr_min );
                r_c = r_0 + max( (r_c - r_0)/2., r_coll * 2. );
                rowpos.push_back( r_c );
                slips.push_back( true );
                is_compact.push_back( false );  //not sure how else to handle this scenario
                r_reset = r_c;

            }
            //next handle the case where the "current" row uses a different template, but the intermediate row does not
            else if( Htemp != Htemp_r2 )
            {
                //the current row causes a template switch. 
                nr++;	//next row
                double r_half = (r_c+r_0)/2.;
				rowpos.push_back(r_half);	//In radial stagger, there's a shifted row halfway between each aligned row
				slips.push_back(false);
				
                //regular spacing on current row, but maintain at least the collision radius
                r_c = max( r_c, r_half + Htemp_r2->getCollisionRadius() );
                rowpos.push_back(r_c);
				slips.push_back(true);
                is_compact.push_back( false );  //not sure how else to handle this scenario
				r_reset = r_c;
            }
            //Is this an inner compact row?
            else if( row_compact )
            {
                nr++;
                r_c = r_0 + r_coll * 2.;
                rowpos.push_back( r_c );
                slips.push_back(true);
                is_compact.push_back( true );
                r_reset = r_c;

            }
            //Check to see if we have a slip plane at the interediate row
			else if( is_slip )	//The intermediate row incurs a slip plane
            {
               
				if(! row_compact_switch){
                    nr++; 
                    
                    //make sure the next 2 rows are at least the collision radius away
                    r_c = max(r_0 + 4.*r_coll, r_c);

				    //Make the row as close as possible and remove shadowed/blocked heliostats. 
				    //Multiply by the _slip_offset factor specified by the user
				    rowpos.push_back((r_c+r_0)/2.);
				    slips.push_back(true);
                    is_compact.push_back(false);
                }
                else
                {
                    r_c = max(r_0 + 2.*r_coll, r_c );
                }

				nr++;
                rowpos.push_back(r_c);
				slips.push_back(false || row_compact_switch);
                is_compact.push_back(false);
				r_reset = r_c;
			}
            //The intermediate row isn't a slip plane, so add the intermediate row and then check the current row
			else{
                //add intermediate row
				nr++;	
				rowpos.push_back((r_c+r_0)/2.);	//In radial stagger, there's a shifted row halfway between each aligned row
				slips.push_back(false);
                is_compact.push_back(false);
                //determine whether current row is a slip plane
				if( r_c/r_reset > _spacing_reset){	//Does the next inline row incurs a slip plane?
					nr++;
					//Put this row as close as possible to the intermediate row. include _slip_offset factor
                    r_c = max(rowpos.back() + 2.*r_coll, r_c );
                    rowpos.push_back( r_c );
					slips.push_back(true);
					r_reset = r_c;
				}
				else{
                    //current row is not a slip plane, just add as-is
					nr++;
					rowpos.push_back( r_c );
					slips.push_back(false);
				}
                is_compact.push_back(false);
			}
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_tht/r_c);
			r_0 = r_c;			
		}
	
		//----Now add heliostats to each row----

		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz;
		int Nhelio = 0, hpr;
        //initialize the heliostat template again
        {
            Point hpos;
            hpos.Set(0., rowpos.front(), 0. );
            Htemp = whichTemplate( _template_rule, hpos );
        }
		double hw = Htemp->getWidth();
        double r_coll = Htemp->getCollisionRadius();

		r_reset = .001; //rowpos.at(0);	//Hold on to the radius where the spacing has reset
	
		azmid = (_accept_max + _accept_min)/2.;	//[rad] The midpoint of the acceptance window
		
		for(i=0; i<nr; i++){	//For each row
			double r_row = rowpos.at(i);	
			
            //Figure out which template to use
            if( slips.at(i) )
            {
                //update template-dependent values
                hloc.Set(r_row,0.,0.);
                Htemp = whichTemplate(_template_rule, hloc);
			    hw = Htemp->getWidth();	//The heliostat width
                r_coll = Htemp->getCollisionRadius();
            }

            if( is_compact.at(i) ){
                daz_init = r_coll * 2.;     //Compact rows have minimum spacing
            }
            else{
			    daz_init = max(r_coll*2, hw*_az_spacing);	//Azimuthal straight-line distance
            }
			//Should this row reset the spacing? It will reset if beyond the spacing ratio limit or a new heliostat width is used
			if( slips.at(i) ){
				az_ang = 2.*atan2(daz_init, 2.*r_row);	//The angular spacing of the heliostats in the row
		
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_accept_max - _accept_min), 2.*pi)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}

			for(j=0; j<hpr; j++){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(i%2);	//heliostat azimuth angle
				HelPos.push_back(Point());
				HelPos.at(Nhelio).x = r_row*sin(haz);
				HelPos.at(Nhelio).y = r_row*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
			}
		}
	}

	return;
}

void SolarField::cornfieldPositions(vector<Point> &HelPos){
	/* 
	Lay out the possible heliostat positions (HelPos) for heliostats arranged in a
	cornfield arrangement. In this configuration, the heliostats are positioned in 
	straight rows, with the positioning staggered between rows. 

	Information for this layout is contained in the following variables:

	_row_spacing_x		//Separation between adjacent heliostats in the X-direction, multiplies heliostat radius
	_row_spacing_y		//Separation between adjacent heliostats in the Y-direction, multiplies heliostat radius
	_xy_field_shape		//Enforced shape of the heliostat field
	_xy_rect_aspect		//Aspect ratio of the rectangular field layout (height in Y / width in X)

	*/

	//get the field limits from the land class
	double rad[2];
	_land.getExtents(rad, _tht);
	double
		radmint = rad[0],
		radmaxt = rad[1];
	//First determine how to split up the templates
	int Nht = _helio_templates.size();	//Number in the radial direction

	//Calculate an upper estimate of the size of the heliostat positions array to avoid resizing all the time
	double 
		r_coll_temp,
		r_coll_min = 9.e9; 
	for(htemp_map::iterator it=_helio_templates.begin(); it != _helio_templates.end(); it++){
		r_coll_temp = it->second->getCollisionRadius();
		if(r_coll_temp < r_coll_min) r_coll_min = r_coll_temp; //minimum collision radius in any combination
	}

	//Estimate the total number of heliostats to reserve in the HelPos array
	int N_max, nxmax, nymax;
	double Lx=0., Ly=0.;
	double 
		ry = 2.*r_coll_temp * _row_spacing_y,
		rx = 2.*r_coll_temp * _row_spacing_x;
	switch(_xy_field_shape)
	{
	case 0:	//Hexagon
		/* 
		The maximum hexagon diameter is governed by the maximum radial value. 

		nx_bar = (radmax*sin(30)+radmax)/2*2
		--> nx_bar = radmax * 1.5 * rx
		ny = 2*radmax*cos(pi/6)/ry
		*/
		N_max = (int)ceil(pow(radmaxt-radmint, 2)*3.*cos(pi/6.)/(rx*ry));
		break;
	case 1:	//Rectangle
	case 2:	//Circular
	{
		/* 
		alpha = _xy_rect_aspect
		theta = atan(alpha)
		Lx = 2*sin(theta)*radmax
		Ly = alpha*Lx
		nx = Lx/rx
		ny = Ly/ry
		*/
		double hyp = sqrt( 1. + _xy_rect_aspect*_xy_rect_aspect );
		//Lx = 2.*sin(atan(1./_xy_rect_aspect))*radmaxt;
		Lx = 2.*radmaxt/hyp;
		Ly = _xy_rect_aspect*Lx;
		nxmax = (int)ceil(Lx/rx);
		nymax = (int)ceil(Ly/ry);
		if(_xy_field_shape == 1){ //rectangle
			N_max = nxmax*nymax;
		}
		else{	//circular
			N_max = (int)ceil(Pi * nxmax * nymax);
		}
		break;
	}
	default:
		_sim_error.addSimulationError("The specified field shape model does not exist",true);
		return;
	}

	//-- now we know the maximum number of heliostats possible in the field
	HelPos.reserve(N_max);



	/* -----------------  Calculate the positions ----------------*/
	double
		y_loc,	//Current row position in Y
		x_offset,	//Stagger offset in X
		x_max,	//maximum position in X
		//x_min,	//minimum x position for the current row
		x_loc,  //current x position
		y_max,
		hex_factor;	//Constant factor used to calculate x_max in hex's
	int 
		//ind_y, //Current row index in Y
		Nhelio;	//Number of heliostats, total


	
	//Initialize values
	x_offset = 999.;	//initialize to nonzero to force into 0 offset on first pass
	y_loc = 0.;
	hex_factor = 0.57735026919; //1./tan(pi/3.);
	Nhelio = 0;
	switch(_xy_field_shape)
	{
	case 0:	//hex
		y_max = cos(pi/6.)*radmaxt;
		break;
	case 1:	//rectangle
		y_max = Ly/2.;
		break;
	case 2:	//circular
		y_max = radmaxt;
		break;
	}
	

	//loop through Y
	while(y_loc <= y_max)
	{
		//Set the x offset
		if(x_offset > 0.){
			x_offset = 0.;
		}
		else{
			x_offset = rx/2.;
		}

		//Calculate the maximum x position
		switch(_xy_field_shape)
		{
		case 0:	//hex
			x_max = radmaxt - hex_factor*y_loc;
			break;
		case 1:	//rect
			x_max = Lx/2.;
			break;
		case 2:	//circular
			x_max = sqrt( pow(radmaxt, 2) - pow(y_loc, 2) );
			break;
		}

			
		//loop through each X position
		x_loc = x_offset;
		while(x_loc <= x_max){
			//check if the x location is within the exclusion area
			if( sqrt(pow(x_loc, 2) + pow(y_loc, 2)) >= radmint ){

				//Add the heliostat position
				HelPos.push_back(Point());
				HelPos.at(Nhelio++).Set(x_loc, y_loc, 0.);
				//Add the -y complement
				if(y_loc>0.){
					HelPos.push_back(Point());
					HelPos.at(Nhelio++).Set(x_loc, -y_loc, 0.);
				}
				//Add the x complement
				if(x_loc > 0.){
					HelPos.push_back(Point());
					HelPos.at(Nhelio++).Set(-x_loc, y_loc, 0.);
					if(y_loc>0.){
						HelPos.push_back(Point());
						HelPos.at(Nhelio++).Set(-x_loc, -y_loc, 0.);
					}
				}
			}
			//increment row X position
			x_loc += rx;
		}


		//increment row Y position
		y_loc += ry;

	}



}

void SolarField::RefactorHeliostatImages(){
	/* 
	This method:
	1. Recalculates the analytical image properties for each heliostat in the field.
	2. Should only be called after the static Hermite terms have been initialized.
	3. Is a truncated form of the Simulate() call, using only the image intercept method.
	4. Does not set any efficiency properties of the simulated heliostats.
	5. Requires that the tracking vectors be previously updated.
	
	Call this method when an existing field geometry is simulated at a different solar 
	position or when aim points are updated.
	*/

	int nh = _heliostats.size();
	for(int i=0; i<nh; i++){
		_flux->imagePlaneIntercept(*_heliostats.at(i), *this, _heliostats.at(i)->getWhichReceiver());
	}

}

bool SolarField::SimulateTime(const string &data){
	/* 
	Simulate a particular date/time for the current solar field geometry.

	The argument "data" is a comma-separated string containing the following items:
	<day of the month>, <hour of the day>, <month (1-12)>, <dni [W/m2]>,<amb. temperature [C]>, <atm. pressure [atm]>, <wind velocity [m/s]>, <weighting factor>

	for example:
	data = "20,12,3,950,25,1,0,1."
	*/
	
	vector<string> vdata = split(data, ",");
	int hour, dom, month;
	double args[5];
	to_integer(vdata.at(0), &dom);
	to_integer(vdata.at(1), &hour);
	to_integer(vdata.at(2), &month);

	for(int i=3; i<8; i++){
		to_double(vdata.at(i), &args[i-3]);
	}
	return SimulateTime(hour, dom, month, args, 5);
}

bool SolarField::SimulateTime(int hour, int day_of_month, int month, double *args, int nargs){
	/* 
	Simulate a particular date/time for the current solar field geometry.

	hour			|	Hour of the day [0,23)
	day_of_month	|	Day of the month [1,31]
	month			|	Month of the year [1,12]
	args[0]			|	DNI [W/m2]
	args[1]			|	Ambient temperature [C]
	args[2]			|	Atmospheric pressure [atm]
	args[3]			|	Wind velocity [m/s]

	*/

	//Convert the day of the month to a day of year
	int doy = _ambient.getDateTimeObj()->GetDayOfYear(2011,month,day_of_month);

	//Calculate the sun position
	_ambient.setDateTime(hour, doy);
	//latitude, longitude, and elevation should be set in the input file
	double azzen[2], az, zen;
	_ambient.calcSunPosition(azzen);
	az = azzen[0]; 
	zen = azzen[1];
	//If the sun is not above the horizon plus a very small amount (to avoid infinite shadows), don't continue
	if( zen > Pi*0.5 )
			return false;
	
	//Simulate field performance
	//args[4] Should contain {dni [W/m2], tdb [C], wind [m/s], pres [mbar], weighting factor [-]};
	Simulate(args, nargs);
	return true;
}

bool SolarField::SimulateTime(double sun_elevation, double sun_azimuth, double *args, int nargs){
	/* 
	Simulate a particular sun position for the current solar field geometry.

	Updates the local values of the sun position.

	sun_elevation	|	solar elevation angle [0,90] degrees
	sun_azimuth		|	solar azimuth angle [-180,180] degrees, N => 0
	args[0]			|	DNI [W/m2]
	args[1]			|	Ambient temperature [C]
	args[2]			|	Atmospheric pressure [atm]
	args[3]			|	Wind velocity [m/s]
	*/

	//If the sun is not in a valid position, don't simulate
	if( sun_elevation <= 0. || sun_elevation > 90. ) return false;

	getAmbientObject()->setSolarPosition(sun_azimuth * d2r, Pi/2.-sun_elevation*d2r);
	Simulate(args, nargs);
	return true;
	
}

void SolarField::Simulate(double args[], int nargs, bool is_layout)
{
	/*
	For a given heliostat field and tower/receiver geometry, simulate the performance of the field. Add the efficiency
	attributes to each heliostat. The method requires that:
	1) The current sun position is set
	2) Receiver height and tower geometry has been defined
	3) The possible field positions have been calculated

	the "args" array contains:
	{dni,  tdb,  vwind, pres, simulation weighting factor}

	*/
	//payment factors
	double payfactor;
	if(_financial.isPaymentFactors()){
		payfactor = _financial.getPaymentFactors()[(_financial.getHourlyTODSchedule())->at(_ambient.getDateTimeObj()->GetHourOfYear())-1];
	}
	else{
		payfactor = 1.;
	}
	if( nargs == 5 )
		payfactor *= args[4];	//simulation weighting factor

	//Check to see if the aimpoints have been set. if not return an error
	if(! getAimpointStatus()){ 
		_sim_error.addSimulationError("The heliostat field aimpoints have not been set or are out of date.", true, true);
		return;
	}

	//Update the estimated receiver thermal efficiency for each receiver
	for(int i=0; i<(int)_receivers.size(); i++){
		_receivers.at(i)->CalculateThermalEfficiency(args[0], _dni_des, args[2], _q_des);
	}
	
	setSimulatedPowerToReceiver(0.);	//reset the simulated power to the receiver
	double dni = args[0];
	int nh = _heliostats.size();
	updateAllTrackVectors();
	//Update the heliostat neighbors to include possible shadowers
	UpdateNeighborList(_helio_extents, is_layout ? 0. : _ambient.getSolarZenith());		//don't include shadowing effects in layout (zenith = 0.)
	//For each heliostat, assess the losses
	Vect *sunvector = _ambient.getSunVector();
	
	//for layout calculations, we can speed things up by only calculating the intercept factor for representative heliostats. (similar to DELSOL).
	if(is_layout && _is_opt_zoning){
		//The intercept factor is the most time consuming calculation. Simulate just a single heliostat in the 
		//neighboring group and apply it to all the rest.
		
		for(int i=0; i<(int)_layout_groups.size(); i++){
			
			Hvector *hg = &_layout_groups.at(i);

			int ngroup = hg->size();

			if(ngroup == 0) continue;

			Heliostat *helios = hg->front(); // just use the first one
			double eta_int = _flux->imagePlaneIntercept( *helios, *this, helios->getWhichReceiver());
			if( eta_int > 1.) eta_int = 1.;
			helios->setEfficiencyIntercept( fmin(eta_int, 1.) );

			for(int k=1; k<ngroup; k++){
				hg->at(k)->setEfficiencyIntercept( eta_int );
				hg->at(k)->CopyImageData( helios );
			}

			
		}
	}
	
	//Simulate efficiency for all heliostats
	for(int i=0; i<nh; i++)
		SimulateHeliostatEfficiency(this, sunvector, _heliostats.at(i), dni, payfactor, is_layout);
	
	


}

void SolarField::SimulateHeliostatEfficiency(SolarField *SF, Vect *sunvector, Heliostat *helios, double &dni, double &payfactor, bool is_layout){
	/*
	Simulate the heliostats in the specified range
	*/
	
	int hid = helios->getId();

	//Cosine loss
	helios->setEfficiencyCosine( Toolbox::dotprod(*sunvector, *helios->getTrackVector()) );
		
	//Attenuation loss
	double slant = helios->getSlantRange(),
	att = SF->getAmbientObject()->calcAttenuation( slant );
	helios->setEfficiencyAtmAtten( att );
	
	Receiver *Rec = helios->getWhichReceiver();

	//Intercept
	if(! (is_layout && SF->isOpticalZoning()) ){	//For layout simulations, the simulation method that calls this method handles image intercept
		double eta_int = SF->getFluxObject()->imagePlaneIntercept(*helios, *SF, Rec);
        if(eta_int != eta_int)
            throw spexception("An error occurred when calculating heliostat intercept factor. Please contact support for help resolving this issue.");
		if(eta_int>1.) eta_int = 1.;
		helios->setEfficiencyIntercept(eta_int);
	}

	//Shadowing and blocking
	double
		shad_tot = 1.,
		block_tot = 1.;
		
	Hvector *neibs = helios->getNeighborList();
	int nn = neibs->size();
	for(int j=0; j<nn; j++){
		if(helios == neibs->at(j) ) continue;	//Don't calculate blocking or shading for the same heliostat
		if(!is_layout) shad_tot += -SF->calcShadowBlock(helios, neibs->at(j), 0);	//Don't calculate shadowing for layout simulations. Cascaded shadowing effects can skew the layout.
		block_tot += -SF->calcShadowBlock(helios, neibs->at(j), 1);
	}
		
	if(shad_tot < 0.) shad_tot = 0.;
	if(shad_tot > 1.) shad_tot = 1.;
	helios->setEfficiencyShading(shad_tot);

	if(block_tot < 0.) block_tot = 0.;
	if(block_tot > 1.) block_tot = 1.;
	helios->setEfficiencyBlocking(block_tot);
	
	//Soiling, reflectivity, and receiver absorptance factors are included in the total calculation
	double eta_rec_abs = Rec->getAbsorptance(); // * eta_rec_acc,
	double eta_total = helios->calcTotalEfficiency();
	double power = eta_total * dni * helios->getArea() * eta_rec_abs;
	helios->setPowerToReceiver( power );
	helios->setPowerValue( power * payfactor * Rec->getReceiverThermalEfficiency());

	return;
	
}

double SolarField::calcShadowBlock(Heliostat *H, Heliostat *HI, int mode){
	/*
	This method takes two heliostats and calculates the interfering or blocking of heliostat 
	"H" by neighbor "HI". 

	interfering is calculated in mode = 0
	Blocking is calculated in mode = 1

	The method assumes that the neighboring heliostats have approximately the same 
	orientation with respect to the sun (i.e. approximately the same tracking vector). This 
	simplifies the calculation.

	The method returns a double value equal to the fraction lost to interference.
	*/

	if(false){ //HI->IsRound()){	//Remove this for now
		//			Round heliostats

		/* 
		The formula for the area of intersection of 2 circles of equal radius is:
		A = 2 R^2 acos[ d/(2R) ] - 1/2 d sqrt(4 R^2 - d^2)
		where:
			R = radius of the circle
			d = distance separating the centroid of the circles

		The shadowing efficiency is equal to (1 - A_intersect/A_heliostat)
		*/
		Point *HIloc, *Hloc;
		//Check to see if the two heliostats are far enough apart that there is no possibility
		//of interfering. This criteria will depend on the solar angle
		Vect *H_inter;
		if(mode == 0){
			//Get the sun vector as the interference direction
			H_inter = _ambient.getSunVector();
		}
		else{
			//Get the tower/receiver vector as the interference direction
			H_inter = H->getTowerVector();
		}

		double zen = acos(H_inter->k);	//The zenith angle for interference

		//Get the interfering heliostat tracking angles
		Vect 
			*HIt = HI->getTrackVector(),	//Interfering heliostat track vector
			*Ht = H->getTrackVector();	//Base heliostat track vector
		
		//Is the heliostat in a position to shadow/block?
		double Hd = HI->getWidth();	//Diameter
		
		//Get locations
		HIloc = HI->getLocation();
		Hloc = H->getLocation();
		
		//Create a vector pointing from the heliostat to the interfering neighbor
		Vect Hnn;
		Hnn.Set(HIloc->x - Hloc->x, HIloc->y - Hloc->y, HIloc->z - Hloc->z);

		//If the heliostat is not in front of the other with respect to the solar position, it also can't shadow
		if( Toolbox::dotprod(Hnn, *H_inter) < 0.) return 0.; 

		//Find the collision point of the centroid of the shadow
		Point hit;
		if( Toolbox::plane_intersect(*Hloc, *Ht, *HIloc, *H_inter, hit) ) {
			//Calculate the distance separating the hit and the heliostat centroid
			Vect vsep;
			vsep.Set( Hloc->x - hit.x, Hloc->y - hit.y, Hloc->z - hit.z );
			double r_sep = Toolbox::vectmag( vsep );
			if(r_sep < Hd){
				//Calculate the shadow overlap area
				double Ax = 2.*pow(Hd/2.,2)*acos(r_sep/Hd) - .5*r_sep*sqrt(pow(Hd,2) - pow(r_sep,2));
				return Ax/(Pi*.25*pow(Hd,2));
			}
		}
		return 0.;

		
	}
	else{
		//			rectangular heliostats
		
		Point *HIloc, *Hloc;
		//Check to see if the two heliostats are far enough apart that there is no possibility
		//of interfering. This criteria will depend on the solar angle
		Vect *H_inter;
		if(mode == 0){
			//Get the sun vector as the interference direction
			H_inter = _ambient.getSunVector();
		}
		else{
			//Get the tower/receiver vector as the interference direction
			H_inter = H->getTowerVector();
		}
		//double zen = acos(H_inter->k);	//The zenith angle for interference

		//Get the interfering heliostat tracking angles
		Vect 
			*HIt = HI->getTrackVector(),	//Interfering heliostat track vector
			*Ht = H->getTrackVector();	//Base heliostat track vector
		double HIh, HIw, HIr;
		HIh = HI->getHeight();
		HIw = HI->getWidth();
		HIr = sqrt(pow(HIh/2.,2) + pow(HIw/2.,2));	//distance from centroid to outermost point
		//Interfering heliostat tracking zenith 
		double HIzen = acos(HIt->k);	//zenith angle 
		//double HIaz = atan2(HIt->i,HIt->j);	//azimuth angle

		/*
		The maximum possible extent between heliostats where interference is a possibility..
		Equals the distance in height between the top edge of the interfering heliostat and the bottom edge of the
		blocked/shadowed heliostat, plus any elevation difference between the heliostats, plus the horizontal distance 
		from tilting the heliostats.
		*/
		HIloc = HI->getLocation();
		Hloc = H->getLocation();
		//double l_max = (HIloc->z - Hloc->z + HIh*sin(HIzen))/tan(pi/2.-zen) + HIh*cos(HIzen);
		double tanpi2zen = H_inter->k/sqrt(H_inter->i*H_inter->i + H_inter->j*H_inter->j);
		double l_max = (HIloc->z - Hloc->z + HIh*sin(HIzen))/tanpi2zen + HIh*HIt->k;
		l_max = fmin(l_max, 100.*HIh);	//limit to a reasonable number

		//Create a vector pointing from the heliostat to the interfering neighbor
		Vect Hnn;
		Hnn.Set(HIloc->x - Hloc->x, HIloc->y - Hloc->y, HIloc->z - Hloc->z);

		//How close are the heliostats?
		double hdist = sqrt(Hnn.i*Hnn.i + Hnn.j*Hnn.j + Hnn.k*Hnn.k);
		
		if(hdist > l_max) return 0.;	//No possibility of interfering, return here.
		//Check for collision radius
		double 
			Hh = H->getHeight(),	//Shaded heliostat height
			Hw = H->getWidth();	//Shaded heliostat width
		double Hr = sqrt(pow(Hh/2.,2) + pow(Hw/2.,2));
		//If the heliostat is not in front of the other with respect to the solar position, it also can't shadow
		if( Toolbox::dotprod(Hnn, *H_inter) < 0.) return 0.; 


		//-----------test
		vector<Point> 
			*cobj = HI->getCornerCoords(),
			ints(2);	//intersection points
		vector<bool> hits(2, false);	//track whether either point hit
		int i;
		for(i=0; i<2; i++){
			if( Toolbox::plane_intersect(*Hloc, *Ht, cobj->at(i), *H_inter, ints.at(i)) ){
				//An intercept on the plane was detected. Is the intercept within the heliostat area?
				hits.at(i) = Toolbox::pointInPolygon(*H->getCornerCoords(), ints.at(i) );
			}
		}
		//Do either of the corners shadow/block the heliostat?
		if(hits.at(0) || hits.at(1)){
			//interfering detected. 
			double dx_inter, dy_inter;
			
			/*
			To calculate the fraction of energy lost, we first transform the intersection point into heliostat coordinates
			so that it's easier to find how the shadow is cast on the heliostat.
			*/
			vector<Point> ints_trans(2);	//Copy of the intersection points for heliostat coordinate transform
			for(i=0; i<2; i++){
				//Express each point of HI in global coords relative to the centroid of H
				//i.e. (ints_trans.x - H->x, ... )
				ints_trans.at(i).Set(ints.at(i).x - Hloc->x, ints.at(i).y - Hloc->y, ints.at(i).z - Hloc->z);

				//First rotate azimuthally back to the north position
				Toolbox::rotation(-H->getAzimuthTrack(), 2, ints_trans.at(i));
				//Next rotate in zenith to get it into heliostat coords. The z component should be very small or zero. 
				//Y components should be relatively close for rectangular heliostats
				Toolbox::rotation(-H->getZenithTrack(), 0, ints_trans.at(i));
			}
			
			//Based on how the image appears, determine interfering. 
			//Recall that after transformation, the positive y edge corresponds to the bottom of the heliostat.
			int which_is_off, which_is_on;
			if(hits.at(0) && hits.at(1)) {
				//Both corners are active in interfering (i.e. the shadow image is contained within the shadowed heliostat
				dy_inter = ( Hh - (ints_trans.at(0).y + ints_trans.at(1).y) ) / (2. * Hh);	//Use the average z position of both points
				dx_inter = fabs(ints_trans.at(0).x - ints_trans.at(1).x ) / Hw;

				return dy_inter * dx_inter;
			}
			else if(hits.at(0)) { 
				//Only the first corner appears in the shadow/blocking image
				which_is_off = 1;
				which_is_on = 0;
			}
			else {
				//Only the second corner appears in the shadow/blocking image
				which_is_off = 0;
				which_is_on = 1;
			}
			
			dy_inter = (Hh/2. - ints_trans.at(which_is_on).y) / Hh;	//The z-interfering component	
			if( ints_trans.at(which_is_off).x > Hw/2. ){ // The shadow image spills off the +x side of the heliostat
				dx_inter = .5 - ints_trans.at(which_is_on).x / Hw;
			}
			else {	//The shadow image spills off the -x side of the heliostat
				dx_inter = ints_trans.at(which_is_on).x / Hw + .5;
			}

			return dy_inter * dx_inter;
		}
		else{	//No interfering
			return 0.;
		}
	}

	//default return
	return 0.;

}

double *SolarField::getPlotBounds(bool use_land){
	/* 
	Returns the field bound extents for plotting based on the field layout 
	
	"use_land" indicates whether the extents should factor in all available land
	
	*/

	//if(use_land){}
	
	//else{
		//Return the array (size = 4) of heliostat position boundaries [xmax,xmin,ymax,ymin]
		return _helio_extents;
	//}


}



void SolarField::updateAllTrackVectors(){
    //update all tracking vectors according to the current sun position
    if(_fluxsim._aim_method == FluxSimData::AIM_STRATEGY::FREEZE )
        return;
    
    int npos = _heliostats.size();
	for(int i=0; i<npos; i++){
		_heliostats.at(i)->updateTrackVector(*_ambient.getSunVector());
	}

}

void SolarField::calcHeliostatShadows(){

	//Calculate the heliostat shadows
	Point P;	//Point on a plane representing the ground
	Vect Nv;	//Vector normal to the ground surface
	Nv.Set(0., 0., 1.);
	int npos = _heliostats.size();
	//Calculate differently if the heliostats are round
	if(_helio_templates.at(0)->IsRound()){

		return;

		//This is all wrong



		Vect *sunvect = _ambient.getSunVector();
		Vect hvect_xy, hvect_z, svect_xy, svect_z;
		double
			fscale_x, fscale_y;
		double
			hdiam = _heliostats.at(0)->getWidth();
		for(int i=0; i<npos; i++){
			P.Set(0.,0., -hdiam/2.*1.1);
			vector<Point> *sc = _heliostats.at(i)->getShadowCoords();
			sc->resize(2);
			/* 
			The shadow coordinates for round heliostats will be:
			0 | x,y,z  of shadow centroid
			1 | <shadow width in x>,<shadow width in y>,0.			
			*/

			Vect *hvect = _heliostats.at(i)->getTrackVector();
			Toolbox::plane_intersect(P, Nv, *_heliostats.at(i)->getLocation(), *sunvect, sc->at(0) );
			//Get relevant vectors
			hvect_xy.Set(hvect->i, hvect->j, 0.);
			Toolbox::unitvect(hvect_xy);
			svect_xy.Set(sunvect->i, sunvect->j, 0.);
			Toolbox::unitvect(svect_xy);
			hvect_z.Set( Toolbox::vectmag(hvect->i, hvect->j, 0.), 0., hvect->k);
			Toolbox::unitvect(hvect_z);
			svect_z.Set( Toolbox::vectmag(sunvect->i, sunvect->j, 0.), 0., sunvect->k);
			Toolbox::unitvect(svect_z);
			//dot products determine shadow scaling
			fscale_x = Toolbox::dotprod(hvect_xy, svect_xy);
			fscale_y = Toolbox::dotprod(svect_z, hvect_z);


			//Calculate the shadow width and height
			sc->at(1).Set( fscale_x*hdiam, fscale_y*hdiam, 0 );
			
		}
	}
	else{
		for(int i=0; i<npos; i++){
			P.Set(0., 0., -_heliostats.at(i)->getHeight()/2.*1.1);
			_heliostats.at(i)->getShadowCoords()->resize(4);
			for(int j=0; j<4; j++){
				Toolbox::plane_intersect(P, Nv, _heliostats.at(i)->getCornerCoords()->at(j), *_ambient.getSunVector(), _heliostats.at(i)->getShadowCoords()->at(j) );
			}
		}
	}
}

void SolarField::calcAllAimPoints(int method, double args[], int nargs){
	/* 
	Method can be:

	0	|	Simple aim points	|	All heliostats point at vertical centerline of their closest receiver point
	1	|	Sigma aim point		|	Heliostats spread depending on image size (sigmas =std dev of image in x and y)
		-> For this method, args[0] = limiting sigma factor. This determines the distance away from the edge
									  in standard deviations of each image.
							args[1] = alternation flag indicating which receiver edge the offset should reference (+1 or -1)
    2   |   Probability         |   
    3   |   Image SIze          |
        ->  args[0] = limiting sigma factor
            args[1] = limiting sigma factor y
            args[2] = First image flag
    4   |   Keep Existing       |
    5   |   Freeze              |
		
	When calculating the aim points, the heliostat images should be updated. Call the flux image updator for methods other
	than simple aim points.

	Uses the local sun position.

	*/

	int nh = _heliostats.size();

	if(method == FluxSimData::AIM_STRATEGY::SIGMA) 
        args[1] = 1.;

	//If the method requires image size, update the analytical estimates of the image
	if(method != FluxSimData::AIM_STRATEGY::SIMPLE){
		//updateAllTrackVectors(); //Do we need to update the tracking vectors to evaluate image size?
		RefactorHeliostatImages();
	}

	//for methods that require sorted heliostats, create the sorted data
	Hvector hsort;
	vector<double> ysize;
	if(method == FluxSimData::AIM_STRATEGY::IMAGE_SIZE){
		//Create a list of heliostats sorted by their Y image size
		int nh = _heliostats.size();
		for(int i=0; i<nh; i++){
			hsort.push_back(_heliostats.at(i));
			ysize.push_back(_heliostats.at(i)->getImageSize()[1]);
		}
		quicksort(ysize,hsort,0,nh-1);	//Sorts in ascending order
	}
	//--
	_sim_info.Reset();
	_sim_info.setTotalSimulationCount(nh);
	int update_every = max(nh/100,1);
	for(int i=0; i<nh; i++){
		
		switch(method)
		{
		case FluxSimData::AIM_STRATEGY::SIMPLE:	//Simple aim points
			//Determine the simple aim point - doesn't account for flux limitations
			_flux->simpleAimPoint(*_heliostats.at(i), *this);
			break;
		case FluxSimData::AIM_STRATEGY::SIGMA:
			args[1] = -args[1];
			_flux->sigmaAimPoint(*_heliostats.at(i), *this, args);
			break;
		case FluxSimData::AIM_STRATEGY::PROBABILITY:
			_flux->probabilityShiftAimPoint(*_heliostats.at(i), *this, args);
			break;
		case FluxSimData::AIM_STRATEGY::IMAGE_SIZE:
			try{
				args[2] = i == 0 ? 1. : 0.;
				_flux->imageSizeAimPoint(*hsort.at(nh-i-1), *this, args, i==nh-1);	//Send in descending order
			}
			catch(...){
				return;
			}
			break;
		case FluxSimData::AIM_STRATEGY::EXISTING:
			//Keep existing aim point, but we still need to update the image plane flux point (geometry may have changed)
        {
            _flux->keepExistingAimPoint(*_heliostats.at(i), *this, 0);
			break;
        }
        case FluxSimData::AIM_STRATEGY::FREEZE:
            //update the aim point based on the movement of the sun and the resulting shift in the reflected image
            _flux->frozenAimPoint(*_heliostats.at(i), *this, args);
            break;
        default:
			return; 
		}		

		//Update the progress bar
		if(i%update_every==0) {
            if(! _sim_info.setCurrentSimulation(i+1) ) break;
        }
	}
	_sim_info.Reset();
	_sim_info.setCurrentSimulation(0);
	setAimpointStatus(true);	//all aimpoints should be up to date

}

int SolarField::getActiveReceiverCount(){
		int n=0;
		for(unsigned int i=0; i<_receivers.size(); i++){ n += _receivers.at(i)->isReceiverEnabled() ? 1 : 0; }
		return n;
}

bool SolarField::parseHeliostatXYZFile(const std::string &filedat, layout_shell &layout ){
	/*
	Take the heliostat layout in text form and parse it into a shell for later use.

	The text lines should be separated by the "\n" newline character, and can be delimited by "," | " " | ";" | "\t"


	Structure:
		0				1				2			3			4					5			6		7			8		9		10		11
	<template (int)> <location X> <location Y> <location Z> <x focal length> <y focal length> <cant i> <cant j> <cant k> <aim X> <aim Y> <aim Z>

	*/
	layout.clear();
	
	//Split by lines
	vector<string> entries = split(filedat, ";"); 
	//if there's only one entry, we used the wrong delimiter. Try "\n"
	int nlines = entries.size();
	if(nlines < 2){
		entries.clear();
		entries = split(filedat, "\n");
		nlines = entries.size();
	}

	//Resize the heliostat vector
	layout.reserve(nlines);
		
	vector<string> data;
	int i, j;
	double loc[3], focal[2], cant[3], aim[3];
		
	//Find the type of delimiter
	string delim = Toolbox::getDelimiter(entries.at(0));
	data.clear();

	for(i=0; i<nlines; i++){
		data = split(entries.at(i), delim);	
		
		//check for empty lines
		if( data.size() < 2 ) continue;
		layout.push_back(layout_obj());

		//If the number of entries is less than 12 (but must be at least 4), append NULL's
		int dsize = data.size();
		if(dsize < 4){
			char fmt[] = "Formatting error\nLine %d in the imported layout is incorrectly formatted. The error occurred while parsing the following text:\n\"%s\"";
			char msg[250];
			sprintf(msg, fmt, i+1, entries.at(i).c_str());
			throw(spexception(msg));  //error!
		}
		else if(dsize < 12){
			for(unsigned int k=dsize; k<12; k++){ data.push_back("NULL"); }
		}
			
		//which template should we use?
		to_integer(data.at(0), &layout.at(i).helio_type);
			
		//Assign the location
		for(j=0; j<3; j++){ to_double(data.at(j+1), &loc[j]); }
		layout.at(i).location.Set(loc[0], loc[1], loc[2]);

		//Assign the focal length
		if(data.at(4)!="NULL"){
			for(j=0; j<2; j++){ to_double(data.at(j+4), &focal[j]); }
			layout.at(i).focal_x = focal[0];
			layout.at(i).focal_y = focal[1];
			layout.at(i).is_user_focus = true;
		}
		else{
			layout.at(i).is_user_focus = false;
		}
			
		//Assign the cant vector unless its null
		if(data.at(6)!="NULL"){
			for(j=0; j<3; j++){ to_double(data.at(j+6), &cant[j]); }
			layout.at(i).cant.Set(cant[0], cant[1], cant[2]);
			layout.at(i).is_user_cant = true;
		}
		else{
			layout.at(i).is_user_cant = false;
		}
			
		//Assign the aim point unless its null
		if(data.at(9)!="NULL"){
			for(j=0; j<3; j++){ to_double(data.at(j+9), &aim[j]); }
			layout.at(i).aim.Set(aim[0], aim[1], aim[2]);
			layout.at(i).is_user_aim = true;
		}
		else{
			layout.at(i).is_user_aim = false;
		}

	}
	return true;
}

int SolarField::calcNumRequiredSimulations(){
	//Based on the solar field settings, how many simulations are impending?
	int nsim;
	if(_des_sim_detail == LAYOUT_DETAIL::NO_FILTER){
		nsim = 1;
	}
	else{
		if(_des_sim_detail == LAYOUT_DETAIL::SUBSET_HOURS){	//Subset of days, all sunlight hours in the selected days
			//Calculate which hours from the days are needed
			//TODO
			throw spexception("Subset hours: Method not currently supported");
		}
		else if(_des_sim_detail == LAYOUT_DETAIL::FULL_ANNUAL){
			if(! _ambient.isWeatherFileLoaded()){_ambient.readWeatherFile();}
			nsim = _ambient.getWeatherData()->size();
		}
		else{
			nsim = _sim_step_data.size();	//If other type of simulation, use the data established in the design select method
		}
	}
	return nsim;
}

double SolarField::getReceiverTotalHeatLoss()
{
    double qloss = 0.;

    for(int i=0; i<(int)_receivers.size(); i++)
    {
        qloss = _receivers.at(i)->getReceiverThermalLoss()*1000.;   //kWt
    }

    return qloss;
}

double SolarField::getReceiverPipingHeatLoss()
{
	double qloss = 0.;

    for(int i=0; i<(int)_receivers.size(); i++)
    {
        qloss = _receivers.at(i)->getReceiverPipingLoss()*1000.;   //kWt
    }

    return qloss;
}


void SolarField::HermiteFluxSimulation(Hvector &helios, bool keep_existing_profile){
	if( ! keep_existing_profile )
		AnalyticalFluxSimulation(helios);
	CalcDimensionalFluxProfiles(helios);
}

void SolarField::AnalyticalFluxSimulation(Hvector &helios)
{
	//Simulate each receiver flux profile (non-dimensional)
	int nrec = _receivers.size();
	for(int n=0; n<nrec; n++){
		if(! _receivers.at(n)->isReceiverEnabled() ) continue;
		FluxSurfaces *surfaces = _receivers.at(n)->getFluxSurfaces();
		for(unsigned int i=0; i<surfaces->size(); i++){
			_flux->fluxDensity(surfaces->at(i), *this, helios,true,true,true);		
		}
	}

}

void SolarField::CalcDimensionalFluxProfiles(Hvector &helios)
{
	/* 
	Take the existing non-dimensional flux profiles in the Receivers::getFluxSurfaces() 
	and alter the values to indicate total dimensional power on the receiver surfaces
	*/

	//DNI
	double dni = _dni_des*0.001;	 //kW/m2

	//Determine the total power delivered from the heliostats. This serves as a normalizing basis.
	double q_to_rec=0.;
	for(unsigned int i=0; i<helios.size(); i++){
		q_to_rec += helios.at(i)->getEfficiencyTotal()*helios.at(i)->getArea()*dni;	//[kW]
	}
	//Receiver surface area
	double Arec = getReceiverTotalArea();	//[m2]
	//Convert to kW/m2
	double q_rec_spec = q_to_rec / Arec;

	//Simulate for each receiver
	int nrec = _receivers.size();
	for(int n=0; n<nrec; n++){
		if(! _receivers.at(n)->isReceiverEnabled() ) continue;
		FluxSurfaces *surfaces = _receivers.at(n)->getFluxSurfaces();
		for(unsigned int i=0; i<surfaces->size(); i++){
			FluxSurface *fs = &surfaces->at(i);
					
			//Take the normalized flux values and multiply to get flux density [kW/m2]
			FluxGrid *grid = fs->getFluxMap();
			double fmax=0.;
            double maxbin=0.;
            double ftot=0.;
            double ftot2 = 0.;
			int nfy = fs->getFluxNY(), nfx = fs->getFluxNX();
			double nfynfx = (double)(nfy*nfx);
            double anode = Arec / nfynfx;
			for(int j=0; j<nfy; j++){
				for(int k=0; k<nfx; k++){
					double *pt = &grid->at(k).at(j).flux;
                    ftot += *pt;
                    if(*pt > maxbin)
                        maxbin = *pt;
                    *pt *= q_to_rec / anode;

					//*pt *= q_rec_spec*nfynfx;
                    ftot2 += *pt;
					if(*pt > fmax)
						fmax = *pt;	
				}
			}
			fs->setMaxObservedFlux(fmax);
		}
	}

}

void SolarField::copySimulationStepData(WeatherData &wdata){
	//the weather data structure contains some pointers that need to be reset. Copy the
	//data and reset the pointers here

	int n = _sim_step_data.size();
	wdata.resizeAll(n);
	double day, hour, month, dni, tdb, pres, vwind, step_weight;
	for(int i=0; i<n; i++){
		_sim_step_data.getStep( i, day, hour, month, dni, tdb, pres, vwind, step_weight);
		wdata.setStep(i, day, hour, month, dni, tdb, pres, vwind, step_weight );
	}

}

void SolarField::getSunPosDesignUser(double pos[2])
{
	pos[0] = _sun_loc_des_az;
	pos[1] = _sun_loc_des_el;
}

bool SolarField::CalcDesignPtSunPosition(int sun_loc_des, double &az_des, double &zen_des)
{
    /* 
    Calculate the design-point sun position given a design point specified by the user.

    sun_loc_des:
        0   Summer Solstice (June 21 N, Dec 21 S)
        1   Equinox (March 20, = Sept 20)
        2   Winter solstice (Dec 21 N, June 21 S)
        3   Zenith (180, 90 elev)
        4   User specified

    Sets:
        az_des      [deg]
        zen_des     [deg]

    Returns:   
        Bool (success)
    */

    int month, day;

        
    bool N_hemis = this->getAmbientObject()->getPlantLatitude() > 0.;

    switch (sun_loc_des)
    {
    case SolarField::SUNPOS_DESIGN::ZENITH:
        az_des = 180.;
        zen_des = 0.;
        return true;
    case SolarField::SUNPOS_DESIGN::USER:
        az_des = _sun_loc_des_az;
        zen_des = 90. - _sun_loc_des_el;
        return true;

    // ^^^^ these methods are done and have returned without calling sun position 

    case SolarField::SUNPOS_DESIGN::SOLSTICE_S:
        month = N_hemis ? 6 : 12;
        day = 21;
        break;
    case SolarField::SUNPOS_DESIGN::EQUINOX:
        month = 3;
        day = 20;
        break;
    case SolarField::SUNPOS_DESIGN::SOLSTICE_W:
        month = N_hemis ? 12 : 6;
        day = 21;
        break;
    default:
		_sim_error.addSimulationError("This design-point sun position option is not available", true); 
        return false;;
    }

    //call the sun position algorithm here

    //Convert the day of the month to a day of year
	int doy = _ambient.getDateTimeObj()->GetDayOfYear(2011,month,day);

	//Calculate the sun position
	_ambient.setDateTime(12., doy);

	double azzen[2];
	_ambient.calcSunPosition(azzen);
	az_des = azzen[0]*r2d; 
	zen_des = azzen[1]*r2d;
	//If the sun is not above the horizon plus a very small amount, fail
	return zen_des < 90.;
}

double SolarField::getAnnualPowerApproximation()
{
	return _estimated_annual_power;
}

double SolarField::getDesignThermalPowerWithLoss(){ return _q_des_withloss; }

double SolarField::getActualThermalPowerWithLoss(){ return _q_to_rec/1.e6; }
// --- clouds ---

void SolarField::clouds::Create(var_map &V, double extents[2]){
	//setVar("is_cloudy", _is_cloudy, V, false);		//Enable simulation for a cloud transient
	setVar("is_cloudy", _is_cloudy, V, false);		//Enable simulation for a cloud transient
	setVar("cloud_shape", _cloud_shape, V, 0);		//Shape used to model the cloud shadow
	setVar("cloud_width", _cloud_width, V, 300.);		//Width of the cloud shape
	setVar("cloud_depth", _cloud_depth, V, 200.);		//Depth of the cloud shape
	setVar("cloud_opacity", _cloud_opacity, V, 0.8, "[0,1]");		//Fraction of DNI obfuscated by a cloud shadow
	setVar("cloud_skew", _cloud_skew, V, 0., "[-180,180]");		//Angle between North and the depth direction (-180 to +180 with clockwise positive)
	setVar("is_cloud_pattern", _is_cloud_pattern, V, true);		//Create a pattern based on the specified cloud
	setVar("cloud_sep_width", _cloud_sep_width, V, 1.);		//Cloud pattern width spacing
	setVar("cloud_sep_depth", _cloud_sep_depth, V, 1.);		//Cloud pattern depth spacing
	setVar("cloud_loc_x", _cloud_loc_x, V, 200.);		//Base location of the cloud(s) relative to the tower position - X dimension
	setVar("cloud_loc_y", _cloud_loc_y, V, 200.);		//Base location of the cloud(s) relative to the tower position - Y dimension
	setVar("is_cloud_symd", _is_cloud_symd, V, true);		//Mirror the cloud pattern below the width axis
	setVar("is_cloud_symw", _is_cloud_symw, V, true);		//Mirror the cloud pattern to the left of the depth axis

	//units
	_cloud_skew *= d2r;

	_all_locs.clear(); //just to be safe...

	//if it's not enabled, we can skip
	if(! _is_cloudy) return;

	//Calculate shadow location(s) here
	if(_is_cloud_pattern && _cloud_shape != clouds::SHAPE::FRONT){
		switch (_cloud_shape)
		{
		case SolarField::clouds::SHAPE::ELLIPTICAL:
		case SolarField::clouds::SHAPE::RECTANGULAR:
		{
			
			//create a point for the initial cloud location
			Point loc = {_cloud_loc_x, _cloud_loc_y, 0.};
			//rotate into original coordinates
			Toolbox::rotation(-_cloud_skew, 2, loc);
			double rcloud_max = max(_cloud_depth, _cloud_width)/2.;
			double dx = _cloud_width * _cloud_sep_width;
			double dy = _cloud_depth * _cloud_sep_depth;
			double
				rfmax = extents[1],
				xp = rfmax - loc.x + rcloud_max + dx/2.,
				xm = _is_cloud_symw ? rfmax + loc.x + rcloud_max : 0.,
				yp = rfmax - loc.y + rcloud_max,
				ym = _is_cloud_symd ? rfmax + loc.y + rcloud_max : 0.;
			
			//add primary point
			//_all_locs.push_back({_cloud_loc_x, _cloud_loc_y, 0.});
			int nry = (int)ceil( (yp + ym) / dy );
			int nrx = (int)ceil( (xp + xm) / dx );

			int ixs = -(int)ceil( xm / dx );
			int iys = -(int)ceil( ym / dy );

			for(int j = iys; j < nry+1; j++){
				double xoffset = j%2==0 ? 0. : dx/2.;
				for(int i = ixs; i < nrx+1; i++){
					Point cloc = {dx * i - xoffset, dy * j, 0.};
					Toolbox::rotation(_cloud_skew, 2, cloc);
					cloc.Add(_cloud_loc_x, _cloud_loc_y, 0.);
					_all_locs.push_back(cloc);
				}
			}
			
			break;
		}
		case SolarField::clouds::SHAPE::FRONT:
			throw spexception("Cannot create a patterned cloud front! Please disable the \"" + V["is_cloud_pattern"].short_desc + "\" checkbox.");
			break;
		default:
			break;
		}
	}
	else{
		//Single cloud
		
		Point p;
		p.x = _cloud_loc_x;
		p.y = _cloud_loc_y;
		p.z = 0.;
		_all_locs.push_back(p);
	}

}

double SolarField::clouds::ShadowLoss(Point &hloc){
	/* 
	Calculate the loss due to cloudiness for this particular location in the field
	*/
	if(! _is_cloudy) return 1.;

	for(vector<Point>::iterator cpt = _all_locs.begin(); cpt != _all_locs.end(); cpt ++){
		//express the heliostat location in the coordinate system of the shadow
		Point hloc_rot = {hloc.x - cpt->x, hloc.y - cpt->y, 0.};
		Toolbox::rotation(-_cloud_skew, 2, hloc_rot);

		bool shadowed = false;
		switch (_cloud_shape)
		{
		case SolarField::clouds::SHAPE::ELLIPTICAL:
		{
			double 
				rx = _cloud_width/2.,
				ry = _cloud_depth/2.;
			if( hloc_rot.x*hloc_rot.x / (rx * rx) + hloc_rot.y*hloc_rot.y / (ry * ry) < 1. )
				shadowed = true;
			break;
		}
		case SolarField::clouds::SHAPE::RECTANGULAR:
			if( fabs(hloc_rot.x) < _cloud_width/2. && abs(hloc_rot.y) < _cloud_depth/2.)
				shadowed = true;

			break;
		case SolarField::clouds::SHAPE::FRONT:
			if( hloc_rot.y > 0. )
				shadowed = true;

			break;
		default:
			break;
		}
		if(shadowed)
			return 1. - _cloud_opacity;
	}


	return 1.;

}

bool SolarField::clouds::isCloudy(){return _is_cloudy;}

