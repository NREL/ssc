#ifndef _AMBIENT_H_
#define _AMBIENT_H_ 1

//#include "math.h"
#include "Toolbox.h"
//#include "solpos00.h"
#include "mod_base.h"
//#include <shared/lib_weatherfile.h>
/*
Compiler note: If solpos or field_core code is not found, 
Add "solpos.lib;field_core.lib;" to PiLOT_GUI project settings, Linker->Input->Additional Dependencies.  
Add "$(SolutionDir)\solpos;" to field_core project settings, C/C++->Additional Include Directories.

The weather file reader is provided in SAM SIM CORE (SSC).
*/

#include <string>
#include <vector>
using namespace std;

//------------------
class Ambient : public mod_base
 {
	string 
		_weather_file;	//location of the weather file
		//_user_sun_file; //location of the user-sunshape file
	int
		_suntype, //Sun type (0=Point sun, 1=Limb-darkened sunshape, 2=Square wave sunshape, 3=user defined)
		_atm_model, //Atmospheric attenuation model {0=25km Barstow, 1 = 5km Barstow, 2 = user defined, 3 = Sengupta}
		_insol_type;	//Model used to determine insolation as a function of time
	matrix_t<double>
		_user_sun,	//2-D array of sunshape points - [[disc angle 1, intensity 1], [... ]] 
		_sun_pos,	//2-d array of sun position (azimth, zenith)
		_atm_coefs;	//1-D Atmospheric attenuation coefficients for user-defined analysis
	//Note that sun angles refer to the centroid of the sun disc
	double
		//pi, d2r, r2d,	//Reused trig factors. Pi, degrees-to-radians, rad-to-deg converts
		_azimuth,	//[rad] Solar azimuth, 0=North, +90=East (TBD convention)
		_zenith,	//[rad] Solar zenith, 0=vertical
		_latitude,	//[rad] plant latitude
		_longitude, //[rad] plant longitude, east positive, west negative
		_elevation, //[m] plant elevation
		_time_zone,	//[hr] Time zone where the plant lies
		_del_h2o,	//[mm H2O]
		_dpres,		//[atm] atmospheric 
		_dni_layout, 	//DNI to use during all layout calculations. CONSTANT model only.
		_sun_rad_limit,	//Half-angle of sunshape size (2.325mrad for Pillbox, 2.73mrad for Gaussian)
		_sun_csr;	//Ratio of solar flux contained in the circumsolar ring over the solar disc flux
	double
		_sim_time_step;	//[sec] The time step to use when calculating the sun position

	DateTime
		_date_time;	//Object with date/time information
	Vect
		_sun_vect;	//(i,j,k) the sun unit vector
	string 
		_wf_type;	//weather file type {SMW, TM2, TM3, EPW, USER}
	WeatherData 
		_wf_data;	//class containing weather data from the weather file
	bool
		_wf_loaded;	//flag to track whether the weather file has been loaded

 public:

	void Create(var_map &V);
	void Clean();

	//Access functions
	void setSunType(int suntype);
	void setUserSun(matrix_t<double> &user_sun);
	void setSolarAzimuth(double az);
	void setSolarZenith(double zen);
	void setSolarPosition(double az, double zen);	//Set both and calculate the sun vector
	void setPlantLocation(double lat, double lon, double tmz);
	void setElevation(double elev);
	void setAtmCoefs(double coefs[], int N);
	bool isWeatherFileLoaded();
	void setSimTimeStep(double step);

	int getSunType();
	matrix_t<double> *getUserSun();
	double getSolarAzimuth();
	double getSolarZenith();
	void getSolarPosition(double &az, double &zen);
	Vect *getSunVector();
	WeatherData *getWeatherData();	//Returns address of the weather file data array
	DateTime *getDateTimeObj();
	void setDateTime(double day_hour, double year_day, double year = 2011.);
	static string getDefaultSimStep();	//d.o.m., hour, month, dni, pressure, wind
	double getPlantLatitude();
	double getPlantLongitude();
	double getTimeZone();
	string getWeatherFilePath();
	double getSunRadLimit();
	double getSunCSR();
	int getInsolType();
	void getInsolParams(double &h2o, double &pres);

	//Calculation methods
	void calcSunPosition(); //Update calculated sun position but don't return any data
	void calcSunPosition(double sp[2]);	//Calculate sun position using the current settings from this object
	void calcSunPosition(double sp[2], const DTobj &dt); //use some local info, some other info
	void calcSunPosition(double sp[2], double lat, double lon, double timezone, const DTobj &dt);	//Calculate with these arguments
	static void calcDaytimeHours(double hrs[2], double lat, double lon, double timezone, const DTobj &dt);
	Vect sunVector(){ return sunVector(_azimuth, _zenith);} //Calculate sun position, use local values for azimuth and zenith
	Vect sunVector(double azimuth, double zenith);	//Calculate sun position given specified az/zen values
	void readWeatherFile();	//overloaded call with defaults from the class
	static bool readWeatherFile(WeatherData &data, string &file_name, Ambient *Amb = (Ambient*)NULL /*Optional - Pointer to class*/);
	double calcAttenuation(double &len);
	void setDefaults();
	static void calcSpacedDaysHours(double lat, double lon, double tmz, int nday, double delta_hr, vector<vector<double> > &utime, vector<int> &uday); //calculate days and times that produce evenly spaced sun positions over the year
	enum CLRSKY_MODEL { MEINEL, HOTTEL, CONSTANT, MOON, ALLEN, WEATHER=-1 };
	double calcInsolation(double azimuth, double zenith); //calculate clear-sky radiation using one of the DELSOL models

 } ;

#endif
