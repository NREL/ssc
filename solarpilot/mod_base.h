#ifndef _MOD_BASE_
#define _MOD_BASE_ 1

/*
Forms the base class for the various components of the solar field. 
These components can use the methods and variable declarations provided here.
*/
#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif

#include <map>
#include <vector>
#include <string>
#include <time.h>
#include "string_util.h"
#include "Toolbox.h"

using namespace std;

#pragma warning(disable:4503)	//warning for name length - VS2010 compiler
#pragma warning(disable:4505)	//warning for removing unused method
 
/* 
Forward-declare the class that will be calling the performance code simulation. This 
is used to provide a callback option from the simulation code to the GUI/calling class
to provide live feedback of simulation progress, errors, etc. 

If some other class is to be used rather than the SolarPILOT GUI (SPFrame), comment
this line and replace it with the name of the substituted class.

Also, the declaration of the variable "_parent" for "simulation_info" should be 
modified with the new class name.
*/
//class SPFrame;	//<------------

class simulation_info
{
	/* 
	This object provides information to the calling program on the status of the 
	simulations underway.
	*/
	void (*_callback)(simulation_info* siminfo, void *data);
	void *_callback_data;

	//void (SPFrame::*_fupdate)(simulation_info* siminfo);	//Pointer to the parent's update function
	//SPFrame *_parent;
	double
		_sim_progress;
	string
		_sim_notice;
	int
		_current_simulation,
		_total_sim_count;
	bool
		_is_active;

public:
	simulation_info();
	
	bool isEnabled();	//Indicates whether any simulation is currently in progress (that's being tracked)
	int getCurrentSimulation();	//Index of the current simulation
	int getTotalSimulationCount();	//Total number of expected simulations in this batch
	double getSimulationProgress();	//Fractional progress [0..1] of the simulation
	void getSimulationInfo(int &current, int &total, double &progress);
	string *getSimulationNotices();	//Returns a pointer to the vector of simulation notices 

	void ResetValues();
	void Reset();

	//Sets
	void setCallbackFunction(void (*updateFunc)(simulation_info* siminfo, void *data), void *cdata);
	void setCurrentSimulation(int val);
	void setTotalSimulationCount(int val);
	void setSimulationProgress(double val);
	void clearSimulationNotices();
	void addSimulationNotice(string &notice);
	void addSimulationNotice(string notice);
	void isEnabled(bool state);
};

class simulation_error
{
	/*
	This class serves as a method for providing the calling program with events and info on 
	simulation (run-time) errors and warnings.
	*/
	//void (SPFrame::*_ferror)();	//Pointer to the parent's error handling function
	//SPFrame *_parent;

	void (*_callback)(simulation_error* simerror, void *data);
	void *_callback_data;

	string _message_log;
	bool _is_connected;		//has this been tied to a parent error handler?
	bool _is_fatal;
	bool _force_display;	//Force the message to pop 
	bool _terminate_status;	//Set by the calling program. Simulations should terminate if true

public:
	simulation_error();

	void setCallbackFunction(void (*errorFunc)(simulation_error* simerror, void *data), void *cdata);

	bool isFatal();
	bool isDisplayNow();
	string *getSimulationErrors();
	bool checkForErrors();

	void Reset();

	//If any error has already been recorded, don't reset the error flag.
	void addSimulationError(string error, bool is_fatal=false, bool force_display=false);
	void addRangeError(double val, string varname, string range);
	void setTerminateStatus(bool do_terminate);
	void clearErrorLog();
	
};


struct spvar
{

	spvar(){};
	unordered_map<int, string> index_map;	//<selection ID, selection name> | Maps the integer associated with a combo choice to the string name
	
	string
		name,	//Formal variable name
		units,	//units for the variable
		value,	//Default value
		ctype,	//Control type
		dattype, //data type {"int", "double", "string", "bool"}
		short_desc, //Short description
		long_desc,	//Long description
		range,	//valid range for the data
		varpath;	//Path to access this variable
	vector<string>
		choices;
	int cselect;	//Current selection for a combo, integer corresponding to the mapped options (not the choices vector)
	bool 
		is_param,	//Is this variable parameterizable?
		is_disabled;	//Is this variable disabled (overridden)?
	int value_int();
	double value_double();
	bool value_bool();
	void set(const double val);
	void set(const int val);
	void set(const bool val);
	
};

//struct var_map : public unordered_map<string, spvar>
//{
//	spvar& at(const string &key){
//		iterator _where = this->lower_bound(key);
//		if(_where == this->end()){
//			char msg[150];
//			sprintf(msg, "Invalid variable map key (%s)", key.c_str());
//			throw exception(msg);
//			
//		}
//		return(_where->second);
//	};
//
//	const spvar& at(const string &key) const{
//		const_iterator _where = this->lower_bound(key);
//		if(_where == this->end()){
//			char msg[150];
//			sprintf(msg, "Invalid variable map key (%s)", key.c_str());
//			throw exception(msg);
//		}
//		return (_where->second);
//	};
//
//	spvar& operator[](const string& key){
//		iterator _where = this->lower_bound(key);
//		if(_where == this->end())
//			_where = this->insert(
//				pair<string, spvar>(
//					key,
//					spvar() ) ).first;
//		return(_where->second);
//	};
//
//};


//Create a standard variable map type
typedef unordered_map<string, spvar> var_map;
//The variable set is an unordered map/map of form [Module type][instance][variable name]
typedef unordered_map<string, map<int,var_map> > var_set;

static spvar *getVarByString(var_set &V, std::string path){
	vector<string> txt = split(path, ".");
	string
		cls = txt.at(0),
		insts = txt.at(1),
		var = txt.at(2);
	int inst;
	to_integer(insts, &inst);

	return &V[cls][inst][var];

};
//----------------
//The land boundary arrays should be described with sets of polygons for inclusions and exclusions
typedef vector<vector<Point> > bounds_array;



class mod_base
{
	//var_map *vars;
	
public:
	string _working_dir;
	double Pi, pi, d2r, r2d;
	mod_base();	//default constructor

	//Set variables
	void setVar(string varname, int &variable, var_map &V, int def = 0, string range=" ");
	void setVar(string varname, double &variable, var_map &V, double def = 0.0, string range=" ");
	void setVar(string varname, string &variable, var_map &V, string def = " ", string range=" ");
	void setVar(string varname, bool &variable, var_map &V, bool def = false, string range=" ");
	void setVar(string varname, matrix_t<double> &variable, var_map &V, string range=" ");
	void setVar(string varname, matrix_t<int> &variable, var_map &V, string range=" ");
	void setVar(string varname, vector<Point> &variable, var_map &V, string range=" ");
	void setVar(string varname, bounds_array &variable, var_map &V);
	void setVar(string varname, WeatherData &WD, var_map &V);
	//void setVar(string varname, void (*fPtr)(int), var_map &V, int def = 0);
	//void setVar(string varname, void (*fPtr)(double), var_map &V, double def = 0.0);
	bool checkRange(string range, int &val, int *flag = NULL);
	bool checkRange(string range, double &val, int *flag = NULL);

	string *getWorkingDir();
	void setWorkingDir(string &dir);

	//error handling
	//Type = "notice","warning","fatal"
	//Message = "Indication of what went wrong"
	//void error(string type, string message){return;};
	//void error_range(int variable, string varname, string range){error_range(double(variable),varname, range);};
	//void error_range(double variable, string varname, string range){
	//	error("ERROR","Variable "+varname+" out of range with value "+to_string(variable)+". Valid range is "+range+".\n");
	//};

};


#endif
