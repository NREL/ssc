#include "Land.h"
#include "math.h"
#include "Toolbox.h"
#include <vector>
#include <math.h>
using namespace std;

//accessors
bounds_array *Land::getInclusions(){return &_inclusions;}
bounds_array *Land::getExclusions(){return &_exclusions;}
	
bool Land::isBoundsScaled(){return _is_bounds_scaled;}
void Land::isBoundsScaled(bool &val){_is_bounds_scaled = val;}
bool Land::isBoundsFixed(){return _is_bounds_fixed;}
void Land::isBoundsFixed(bool &val){_is_bounds_fixed = val;}
bool Land::isBoundsArray(){return _is_bounds_array;}
void Land::isBoundsArray(bool &val){_is_bounds_array = val;}

void Land::Create(var_map &V)
{
	setVar("max_scaled_rad", _max_scaled_rad, V, 7.5, "(0.,100.]");		//Maximum radius (in units of tower height) for positioning of the heliostats
	setVar("min_scaled_rad", _min_scaled_rad, V, 0.75, "[0.,100.]");		//Minimum radius (in units of tower height) for positioning of the heliostats
	setVar("max_fixed_rad", _max_fixed_rad, V, 1500., "(0.,9e99]");		//Outer land boundary for circular land plot
	setVar("min_fixed_rad", _min_fixed_rad, V, 100., "(0.,9e99]");		//Inner land boundary for circular land plot
	setVar("topo_grid", _topo_grid, V);		//Regular grid of the land topology of the solar field
	setVar("inclusions", _inclusions, V);		//Vector of arrays that specify the regions of land to include in the heliostat layout
	setVar("exclusions", _exclusions, V);		//Vector of arrays that specify the regions of land to exclude in the heliostat layout
	setVar("is_bounds_scaled", _is_bounds_scaled, V, true);		//Land boundary scales with tower hight value
	setVar("is_bounds_fixed", _is_bounds_fixed, V, false);		//Land boundary has fixed limits (not more than | not less than)
	setVar("is_bounds_array", _is_bounds_array, V, false);		//Land boundary is specified by points array

}

void Land::Clean(){
	/* Clean variables that shouldn't be available after a new field geometry is created. */
	_inclusions.clear();
	_exclusions.clear();
	_topo_grid.clear();

}

bool Land::InBounds(Point &P, double tht){
	//figure out whether the given point is inside the land area described by _boundary
	bool test = true;
	double prad = sqrt( pow(P.x, 2) + pow(P.y, 2) ); //radial position of the point
		
	if(_is_bounds_scaled){	//Does the point lie within the limits scaling with tower height?
		test = (prad >= (tht*_min_scaled_rad) && prad <= (tht*_max_scaled_rad) );
		if(! test) return false;
	}
	if(_is_bounds_fixed){	//Does the point also lie within the fixed limits?
		test = test && (prad >= _min_fixed_rad && prad <= _max_fixed_rad);
		if(! test) return false;
	}
	if(_is_bounds_array){
		//Test all of the exclusions polygons first
		for(unsigned int i=0; i<_exclusions.size(); i++){ 
			if( Toolbox::pointInPolygon(_exclusions.at(i), P) ) return false;	//if the point is in any exclusion, stop
		}
		//Test all of the inclusions polygons. The point must lie in one or more inclusion.
		bool intest = false;
		for(unsigned int i=0; i<_inclusions.size(); i++){
			if( Toolbox::pointInPolygon(_inclusions.at(i), P) ){ intest = true; break;}
		}
		test = test && intest;
	}
	
	return test;
}

void Land::getExtents(double rval[], var_set &V)
{
	/*
	**CALL FOR GUI**
	Take the rval[2] array (output) and the variable map structure from the interface and calculate {radmin, radmax}.

	This returns the min and max radial distance of the heliostat field from the tower location.
	The land bounds can be specified using up to three constraints: 1 - scale with tower height, 
	2 - fixed distances, 3 - user-specified polygon.

	This method considers all methods used and enforces the bounds based on satisfaction of all of
	the active criteria.
	
	This array takes an optional argument "tht" which multiplies by the scaled radii if appropriate.
	By default tht = 1.0
	The method returns an array size=2: {double min, double max}
	*/
	double radmin = NULL, radmax=NULL;
	double tht;  to_double(V["solarfield"][0]["tht"].value, &tht);
	bool is_bounds_scaled; to_bool(V["land"][0]["is_bounds_scaled"].value, is_bounds_scaled);
	bool is_bounds_fixed; to_bool(V["land"][0]["is_bounds_fixed"].value, is_bounds_fixed);
	bool is_bounds_array; to_bool(V["land"][0]["is_bounds_array"].value, is_bounds_array);
	
	if(is_bounds_scaled){
		double min_scaled_rad; to_double(V["land"][0]["min_scaled_rad"].value, &min_scaled_rad);
		double max_scaled_rad; to_double(V["land"][0]["max_scaled_rad"].value, &max_scaled_rad);
		radmin = min_scaled_rad * tht;
		radmax = max_scaled_rad * tht;
	}
	if(is_bounds_fixed){
		double min_fixed_rad; to_double(V["land"][0]["min_fixed_rad"].value, &min_fixed_rad);
		double max_fixed_rad; to_double(V["land"][0]["max_fixed_rad"].value, &max_fixed_rad);
		if(min_fixed_rad > radmin || radmin == NULL) radmin = min_fixed_rad;	//Only change if the fix min radius is larger than the previous bound
		if(max_fixed_rad < radmax || radmax == NULL) radmax = max_fixed_rad;	//Only change if the fix max radius is smaller than the previous bound
	}
	if(is_bounds_array){
		//Get the bounds array
		bounds_array 
			inclusions,	//vector of polygons describing the land to be included
			exclusions;	//vector of polygons describing the land to be excluded

		setVar("inclusions", inclusions, V["land"][0]);
		setVar("exclusions", exclusions, V["land"][0]);
		//Find the maximum radius depending on the inclusions vectors
		double rad, trmax = -1.;
		for(unsigned int i=0; i<inclusions.size(); i++){
			//For each polygon in the inclusions
			for(unsigned int j=0; j<inclusions.at(i).size(); j++){
				rad = sqrt( pow(inclusions.at(i).at(j).x, 2) + pow(inclusions.at(i).at(j).y, 2) );
				if(fabs(rad) > trmax) trmax = rad;
			}
		}
		if(trmax < 0.) trmax = tht*7.5;	//use the default if nothing is set
		if(trmax < radmax || radmax == NULL) radmax = trmax;

		//Find the minimum radius depending on the exclusions vector
		//values for finding the minimum radius
		double trmin = 9.e9; 
		Point T, pt1, N;
		T.Set(0.,0.,0.);	//Tower location
		for(unsigned int i=0; i<_inclusions.size(); i++){	//For each polygon in the inclusions
			//first check whether the tower lies within the polygon, in which case the minimum radius is zero
			if( Toolbox::pointInPolygon(_inclusions.at(i), T) ){
				trmin = 0.;
				break;
			}
					
			int nincpt = _inclusions.at(i).size();
			for(int j=0; j<nincpt; j++){
				
				//Find the minimum radius depending on the inclusions vectors
				if(j<nincpt-1){
					pt1.Set(_inclusions.at(i).at(j+1));
				}
				else{
					pt1.Set(_inclusions.at(i).at(0));
				}

				//Find the closest point on the line defined by pt1 and pt0 to 'T'.
				Toolbox::line_norm_intersect(_inclusions.at(i).at(j), pt1, T, N, rad);
				if(fabs(rad < trmin)) trmin = rad;

			}
		}
		
		//Adjust the minimum radius depending on the exclusions 
		Point ex1;
		double excheck = 9.e9;
		for(unsigned int i=0; i<_exclusions.size(); i++){	//For each polygon in the exclusions
			
			//check whether the tower lies within the polygon. If not, we don't need to adjust
			if(! Toolbox::pointInPolygon(_exclusions.at(i), T) ) continue;

			int nex = _exclusions.at(i).size();
			for(int j=0; j<nex; j++){
				
				if(j<nex-1) {
					ex1.Set(_exclusions.at(i).at(j+1));
				}
				else{
					ex1.Set(_exclusions.at(i).at(0));
				}
				//Find the closest point on the line defined by ex0 and ex1 to 'T'. This point
				//is 'N' with a distance 'rad' from T.
				Toolbox::line_norm_intersect(_exclusions.at(i).at(j), ex1, T, N, rad);
				if(fabs(rad) < excheck) excheck = rad;
			}
		}
		if(excheck > trmin && excheck < 9.e9) trmin = excheck;		if(trmin > radmax) trmin = 0.001;	//Use a small number larger than zero if nothing is set
		if(trmin > radmin || radmin == NULL) radmin = trmin;
	}
	rval[0] = radmin;
	rval[1] = radmax;
	
}

void Land::getExtents(double rval[], double tht)
{
	/*
	This returns the min and max radial distance of the heliostat field from the tower location.
	The land bounds can be specified using up to three constraints: 1 - scale with tower height, 
	2 - fixed distances, 3 - user-specified polygon.

	This method considers all methods used and enforces the bounds based on satisfaction of all of
	the active criteria.
	
	This array takes an optional argument "tht" which multiplies by the scaled radii if appropriate.
	By default tht = 1.0
	The method returns an array size=2: {double min, double max}
	*/
	double radmin = NULL, radmax=NULL;
	
	if(_is_bounds_scaled){
		radmin = _min_scaled_rad * tht;
		radmax = _max_scaled_rad * tht;
	}
	if(_is_bounds_fixed){
		if(_min_fixed_rad > radmin || radmin == NULL){radmin = _min_fixed_rad;}	//Only change if the fix min radius is larger than the previous bound
		if(_max_fixed_rad < radmax || radmax == NULL){radmax = _max_fixed_rad;}	//Only change if the fix max radius is smaller than the previous bound
	}
	if(_is_bounds_array){

		//values for finding the maximum radius
		double rad, trmax = -1.;
		for(unsigned int i=0; i<_inclusions.size(); i++){	//For each polygon in the inclusions
			int nincpt = _inclusions.at(i).size();
			for(int j=0; j<nincpt; j++){
				
				//Find the maximum radius depending on the inclusions vectors
				rad = sqrt( pow(_inclusions.at(i).at(j).x, 2) + pow(_inclusions.at(i).at(j).y, 2) );
				if(fabs(rad) > trmax) trmax = rad;
			}
		}
		if(trmax < 0.) trmax = tht*7.5;	//use the default if nothing is set
		if(trmax < radmax || radmax == NULL){radmax = trmax;}	

		//values for finding the minimum radius
		double trmin = 9.e9; 
		Point T, pt1, N;
		T.Set(0.,0.,0.);	//Tower location
		for(unsigned int i=0; i<_inclusions.size(); i++){	//For each polygon in the inclusions
			//first check whether the tower lies within the polygon, in which case the minimum radius is zero
			if( Toolbox::pointInPolygon(_inclusions.at(i), T) ){
				trmin = 0.;
				break;
			}
					
			int nincpt = _inclusions.at(i).size();
			for(int j=0; j<nincpt; j++){
				
				//Find the minimum radius depending on the inclusions vectors
				if(j<nincpt-1){
					pt1.Set(_inclusions.at(i).at(j+1));
				}
				else{
					pt1.Set(_inclusions.at(i).at(0));
				}

				//Find the closest point on the line defined by pt1 and pt0 to 'T'.
				Toolbox::line_norm_intersect(_inclusions.at(i).at(j), pt1, T, N, rad);
				if(fabs(rad < trmin)) trmin = rad;

			}
		}
		
		//Adjust the minimum radius depending on the exclusions 
		Point ex1;
		double excheck = 9.e9;
		for(unsigned int i=0; i<_exclusions.size(); i++){	//For each polygon in the exclusions
			
			//check whether the tower lies within the polygon. If not, we don't need to adjust
			if(! Toolbox::pointInPolygon(_exclusions.at(i), T) ) continue;

			int nex = _exclusions.at(i).size();
			for(int j=0; j<nex; j++){
				
				if(j<nex-1) {
					ex1.Set(_exclusions.at(i).at(j+1));
				}
				else{
					ex1.Set(_exclusions.at(i).at(0));
				}
				//Find the closest point on the line defined by ex0 and ex1 to 'T'. This point
				//is 'N' with a distance 'rad' from T.
				Toolbox::line_norm_intersect(_exclusions.at(i).at(j), ex1, T, N, rad);
				if(fabs(rad) < excheck) excheck = rad;
			}
		}
		if(excheck > trmin && excheck < 9.e9) trmin = excheck;

		if(trmin > radmax) trmin = 0.001;	//Use a small number larger than zero if nothing is set
		if(trmin > radmin || radmin == NULL) radmin = trmin;
	}
	rval[0] = radmin;
	rval[1] = radmax;
}

void Land::getRadialExtents(double rval[2], double tht){
	/* 
	Sets the values of rval equal to the [min radius, max radius] of the field. This ONLY APPLIES to the 
	radial boundary settings and not to the polygonal boundary settings. If no radial boundaries are used,
	return [-1,-1].
	*/
	double radmin = NULL, radmax=NULL;
	
	if(_is_bounds_scaled){
		radmin = _min_scaled_rad * tht;
		radmax = _max_scaled_rad * tht;
	}
	if(_is_bounds_fixed){
		if(_min_fixed_rad > radmin || radmin == NULL){radmin = _min_fixed_rad;}	//Only change if the fix min radius is larger than the previous bound
		if(_max_fixed_rad < radmax || radmax == NULL){radmax = _max_fixed_rad;}	//Only change if the fix max radius is smaller than the previous bound
	}
	
	rval[0] = radmin;
	rval[1] = radmax;

	if(radmin == NULL) rval[0] = -1.;
	if(radmax == NULL) rval[1] = -1.;



}

double Land::calcPolyLandArea(){
	//Simple summation of polygon inclusions minus exclusions

	//First add all the inclusions together
	double area = 0.;

	for(unsigned int i=0; i<_inclusions.size(); i++){
		int np = _inclusions.at(i).size();
		int j=np-1;
		for(int k=0; k<np; k++){
			Point 
				*pj = &_inclusions.at(i).at(j), 
				*pk = &_inclusions.at(i).at(k);
			area += (pj->x + pk->x)*(pj->y - pk->y)/2.;
			j = k;
		}
	}
	area = fabs(area);

	//Now subtract the area of the exclusions
	double excs = 0.;
	for(unsigned int i=0; i<_exclusions.size(); i++){
		int np = _exclusions.at(i).size();
		int j=np-1;
		for(int k=0; k<np; k++){
			Point 
				*pj = &_exclusions.at(i).at(j), 
				*pk = &_exclusions.at(i).at(k);
			excs += (pj->x + pk->x)*(pj->y - pk->y)/2.;
			j = k;
		}
	}	
	excs = fabs(excs);

	return area-excs;

}
