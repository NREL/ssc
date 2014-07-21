#include "mod_base.h"
#include <algorithm>

//------ simulation info --------

simulation_info::simulation_info(){ 
	_is_active = false; 
	Reset(); 
	_callback = 0;
	_callback_data = 0;
}
	
bool simulation_info::isEnabled(){return _is_active;}	//Indicates whether any simulation is currently in progress (that's being tracked)
int simulation_info::getCurrentSimulation(){return _is_active ? _current_simulation : 0;}	//Index of the current simulation
int simulation_info::getTotalSimulationCount(){return _is_active ? _total_sim_count : 0;}	//Total number of expected simulations in this batch
double simulation_info::getSimulationProgress(){return _is_active ? _sim_progress : 0.;}	//Fractional progress [0..1] of the simulation
void simulation_info::getSimulationInfo(int &current, int &total, double &progress){ 
	if(!_is_active) return;
	current = _current_simulation;
	total = _total_sim_count;
	progress = _sim_progress;
};
string *simulation_info::getSimulationNotices(){return &_sim_notice;}	//Returns a pointer to the vector of simulation notices 

void simulation_info::ResetValues(){
	_current_simulation = 0; 
	_total_sim_count = 0;
	_sim_progress = 0.;
};
void simulation_info::Reset(){
	_current_simulation = 0; 
	_total_sim_count = 0;
	_sim_progress = 0.;
	_sim_notice.clear();
};

//Sets
void simulation_info::setCallbackFunction(void (*updateFunc)(simulation_info* siminfo, void *data), void *cdata){
	_callback = updateFunc;
	_callback_data = cdata;
	_is_active = true;
}

void simulation_info::setCurrentSimulation(int val)
{
	if(!_is_active) return; 
	_current_simulation = val; 
	(*_callback)(this, _callback_data);
	//(*_parent.*_fupdate)(this);
}

void simulation_info::setTotalSimulationCount(int val)
{
	if(!_is_active) return; 
	_total_sim_count = val; 
	(*_callback)(this, _callback_data);
	//(*_parent.*_fupdate)(this);
}

void simulation_info::setSimulationProgress(double val)
{
	if(!_is_active) return; 
	_sim_progress = val; 
	(*_callback)(this, _callback_data);
	//(*_parent.*_fupdate)(this);
}

void simulation_info::clearSimulationNotices()
{
	if(!_is_active) return; 
	_sim_notice.clear();
}

void simulation_info::addSimulationNotice(string &notice)
{
	if(!_is_active) return; 
	_sim_notice =  notice; 
	(*_callback)(this, _callback_data);
	//(*_parent.*_fupdate)(this);
}

void simulation_info::addSimulationNotice(string notice)
{
	if(!_is_active) return; 
	_sim_notice = notice; 
	(*_callback)(this, _callback_data);
	//(*_parent.*_fupdate)(this);
}

void simulation_info::isEnabled(bool state){_is_active = state;}
//-------------------------------

mod_base::mod_base(){
	//Reused trig values
	pi=acos(-1.);
	Pi = pi;
	r2d=180./pi;
	d2r=1./r2d;
}


//Setvars
void mod_base::setVar(string varname, int &variable, var_map &V, int def, string range)
{
	if(V.find(varname) != V.end()) {
		to_integer(V[varname].value, &variable);
		//if(!checkRange(range, variable)){error_range(variable, varname, range);};
	} 
	else 
	{
		variable = def;
	}
};

void mod_base::setVar(string varname, double &variable, var_map &V, double def, string range)
{
	if(V.find(varname) != V.end()) {
		to_double(V[varname].value, &variable);
		//if(!checkRange(range, variable)){error_range(variable, varname, range);};
	} 
	else {
		variable = def;
	}
};

void mod_base::setVar(string varname, string &variable, var_map &V, string def, string range)
{
	if(V.find(varname) != V.end()) {
		variable = V[varname].value;
	} 
	else {
		variable = def;
	}
};

void mod_base::setVar(string varname, bool &variable, var_map &V, bool def, string range)
{
	if(V.find(varname) != V.end()) {
		to_bool(V[varname].value, variable);
	}
	else {
		variable = def;
	}
};

void mod_base::setVar(string varname, matrix_t<double> &variable, var_map &V, string range)
{
	//converts a string with comma-separated values in sequence into a 2-D matrix of doubles
	if(V.find(varname) != V.end()) {
		vector<string> content = split(V[varname].value, ";");
		int nrows = content.size();
		if(nrows == 0) { variable.resize_fill(1,2,0.0); return; }
		vector<string> line;
		line = split(content.at(0), ",");
		int rowlen = line.size();
		variable.resize(nrows, rowlen);
		for (int i=0; i<nrows; i++){
			line = split(content.at(i), ",");
			for (int j=0; j<rowlen; j++){
				to_double(line.at(j), &variable.at(i, j));
			}
		}
	}
	else {
		variable.resize_fill(1,2,0.0);
	}
}

void mod_base::setVar(string varname, matrix_t<int> &variable, var_map &V, string range)
{
	//converts a string with comma-separated values in sequence into a 2-D matrix of integers
	if(V.find(varname) != V.end()) {
		vector<string> content = split(V[varname].value, ";");
		int nrows = content.size();
		if(nrows == 0){ variable.resize_fill(1,2,0); return; }
		vector<string> line;
		line = split(content.at(0), ",");
		int rowlen = line.size();
		variable.resize(nrows, rowlen);
		for (int i=0; i<nrows; i++){
			line = split(content.at(i), ",");
			for (int j=0; j<rowlen; j++){
				to_integer(line.at(j), &variable.at(i, j));
			}
		}
	}
	else {
		variable.resize_fill(1,2,0);
	}
}	

void mod_base::setVar(string varname, vector<Point> &variable, var_map &V, string range)
{
	//splits a set of 3d points into a vector<Point>
	//should be [P]x1,y1,z1[P]x2,y2,z2...
	if(V.find(varname) != V.end()) {
		vector<string> content = split(V[varname].value, "[P]");
		vector<string> line;
		double x, y, z;
		int nrows = content.size();
		variable.resize(nrows);
		for(int i=0; i<nrows; i++) {
			//split each text by comma
			line = split(content.at(i), ",");
			to_double(line.at(0), &x);
			to_double(line.at(1), &y);
			to_double(line.at(2), &z);
			variable.at(i).Set(x, y, z);
		}
	}
	else {
		variable.resize(1);
		variable.at(0).Set(0.,0.,0.);
	}
}

void mod_base::setVar(string varname, bounds_array &variable, var_map &V){
	//Specifically loads the field boundary arrays
	//The format should be [POLY][P]x1,y1,z1[P]x2,y2,z2...[POLY][P]...
	if(V.find(varname) != V.end()) {
		vector<string> spolys = split(V[varname].value, "[POLY]");
		vector<string> line, pnt;
		double x, y, z;
		int npoly = spolys.size();
		variable.resize(npoly);
		for(int i=0; i<npoly; i++){
			line.clear();
			line = split(spolys.at(i), "[P]");
			int npt = line.size();	//The number of points in the polygon
			variable.at(i).resize(npt);	//Resize the polygon container

			for(int j=0; j<npt; j++){
				pnt = split(line.at(j), ",");	//Split by comma
				to_double(pnt.at(0), &x);
				to_double(pnt.at(1), &y);
				to_double(pnt.at(2), &z);
				variable.at(i).at(j).Set(x, y, z);
			}
		}

	}
	else{
		variable.resize(1);
		variable.at(0).resize(1);
		variable.at(0).at(0).Set(0.,0.,0.);
	}

}

void mod_base::setVar(string varname, WeatherData &WD, var_map &V){
	//Converts a string with comma-separated values in sequence into a WeatherData object. Each row of data is separated with a [P] flag.
	if(V.find(varname) != V.end()){
		vector<string>
			vals,
			entries = split(V[varname].value, "[P]");
		int nrows = entries.size();
		int nv, i, j;
		WD.resizeAll(nrows, 0.0);
		vector<vector<double>*> *wdvars = WD.getEntryPointers();

		for(i=0; i<nrows; i++){
			vals = split(entries.at(i), ",");
			nv = min(vals.size(), wdvars->size());
			for(j=0; j<nv; j++){
				to_double(vals.at(j), &wdvars->at(j)->at(i));
			}
		}
	}
	else{
		WD.resizeAll(1);
		WD.setStep(0, 81., 12., 950.,1.);
	}
}

bool mod_base::checkRange(string range, double &val, int *flag)
{
	//take range string of form:
	// {dlow,dhi} 
	// where {} can be replaced by ( ), [ ], ( ], [ )

	//parse the string
	vector<string> t1 = split(range, ",");
	if(t1.size()<2) return true;

	string lop, rop, ops, ls, rs;
	ls = t1.at(0);
	rs = t1.at(1);
	lop = ls.at(0);
	rop = rs.at(rs.size()-1);
	//Convert range values to doubles
	double lval, rval;
	to_double(ls.erase(0,1), &lval);
	to_double(rs.erase(rs.size()-1,1), &rval);
	//flags for information to return
	int tflag=-1;	//return the type of range applied (i.e. less than, greater than | less than or equal to, greater than, etc)
	bool retflag=false;	//Is the boundary satisfied?

	ops = lop+rop;
	if(ops == " "){return true;}	//no info, don't check
	else if(ops == "()"){if(val > lval && val < rval) {retflag = true; tflag=1;}}
	else if(ops == "[)"){if(val >= lval && val < rval) {retflag = true; tflag=2;}}
	else if(ops == "(]"){if(val > lval && val <= rval) {retflag = true; tflag=3;}}
	else if(ops == "[]"){if(val >= lval && val <= rval) {retflag = true; tflag=4;}}
	else{retflag = true;}

	if(flag != NULL) *flag = tflag;
	return retflag;	//boundary not satisfied by any previous consideration

}

bool mod_base::checkRange(string range, int &val, int *flag)
{
	double dval = double(val); 
	return checkRange(range, dval, flag);
};

string *mod_base::getWorkingDir(){return &_working_dir;}
void mod_base::setWorkingDir(string &dir){_working_dir = dir;}

//--------------
//   var data
//--------------

bool var_data::value_bool(){
	if(dattype == "bool")
		return lower_case(value) == "true";
	else
		return false;
}

int var_data::value_int(){
	if(dattype == "int"){
		int d;
		to_integer(value, &d);
		return d;
	}
	else{ return 0; }
}

double var_data::value_double(){
	if(dattype == "double"){
		double d;
		to_double(value, &d);
		return d;
	}
	else{ return 0.; }
}

#include <sstream>
template<typename T> string to_string(const T &value) {
	ostringstream x;
	x << value;
	return x.str();
}

void var_data::set(double val){
	value = to_string(val);
}

void var_data::set(int val){
	value = to_string(val);
}

void var_data::set(bool val){
	value = val ? "true" : "false";
}


//-------------------- simulation error ------------
simulation_error::simulation_error(){ 
	_callback = 0; 
	_callback_data = 0;
	_is_fatal = false; 
	_force_display = false;
	_terminate_status = false;
	_is_connected = false;
	_message_log.clear();
}

void simulation_error::setCallbackFunction(void (*errorFunc)(simulation_error* sim_error, void *data), void *cdata){
	_callback = errorFunc;
	_callback_data = cdata;
	_is_connected = true;
}

bool simulation_error::isFatal(){return _is_fatal;}
bool simulation_error::isDisplayNow(){return _force_display;}
string *simulation_error::getSimulationErrors(){return &_message_log;}
bool simulation_error::checkForErrors(){return _terminate_status || _is_fatal;}

void simulation_error::Reset(){
	_is_fatal = false; 
	_terminate_status = false;
	_force_display = false;	
	_message_log.clear(); 
}
//If any error has already been recorded, don't reset the error flag.
void simulation_error::addSimulationError(string error, bool is_fatal, bool force_display){
	if(! _is_connected ) return;	//only deal with this if the object has been connected to a callback

	_is_fatal = _is_fatal ? true : is_fatal; 
	_force_display = _force_display ? true : force_display;
	_message_log.append(error);
	(*_callback)(this, _callback_data);
	//(*_parent.*_ferror)();
}
void simulation_error::addRangeError(double val, string varname, string range){
	char fmt[] = "Variable %s is out of range with value %f. The valid range is %s.\n";
	char msg[200];
	sprintf(msg, fmt, varname.c_str(), val, range.c_str());
	addSimulationError(msg, true, true);
}
void simulation_error::setTerminateStatus(bool do_terminate){_terminate_status = do_terminate;}
void simulation_error::clearErrorLog(){_message_log.clear();}
