#include <math.h>
#include <vector>
#include <stdio.h>

#include "Ambient.h"
#include "solpos00.h"
#include "math.h"
#include "Toolbox.h"
#include <shared/lib_weatherfile.h>

using namespace std;




//-------------------------------AMBIENT CLASS------------------------------------

//Access functions
void Ambient::setSunType(int suntype){_suntype = suntype;};
void Ambient::setUserSun(matrix_t<double> &user_sun){_user_sun = user_sun;}
void Ambient::setSolarAzimuth(double az){_azimuth = az;}
void Ambient::setSolarZenith(double zen){_zenith = zen;}
void Ambient::setSolarPosition(double az, double zen){
	//Set both and calculate the sun vector
	_azimuth = az; 
	_zenith = zen; 
	sunVector();
}	

void Ambient::setPlantLocation(double lat, double lon, double tmz){
	_latitude = lat; 
	_longitude = lon; 
	_time_zone = tmz;
}

void Ambient::setElevation(double elev){_elevation = elev;}
bool Ambient::isWeatherFileLoaded(){return _wf_loaded;}
void Ambient::setSimTimeStep(double step){ _sim_time_step = step;};

int Ambient::getSunType(){return _suntype;}
matrix_t<double> *Ambient::getUserSun(){return &_user_sun;}
double Ambient::getSolarAzimuth(){return _azimuth;}
double Ambient::getSolarZenith(){return _zenith;}
void Ambient::getSolarPosition(double &az, double &zen){
	az = _azimuth; 
	zen = _zenith;
}

Vect *Ambient::getSunVector(){ return &_sun_vect; }
WeatherData *Ambient::getWeatherData(){ return &_wf_data;}	//Returns address of the weather file data array
DateTime *Ambient::getDateTimeObj(){return &_date_time;}
string Ambient::getDefaultSimStep(){return "20,12,3,950,25,1,0,1";}	//d.o.m., hour, month, dni, pressure, wind, weight factor
double Ambient::getPlantLatitude(){return _latitude; }
double Ambient::getPlantLongitude(){return _longitude; }
double Ambient::getTimeZone(){return _time_zone; }
string Ambient::getWeatherFilePath(){return _weather_file;}
double Ambient::getSunRadLimit(){return _sun_rad_limit;}
double Ambient::getSunCSR(){return _sun_csr;}
int Ambient::getInsolType(){return _insol_type;}
void Ambient::getInsolParams(double &h2o, double &pres){
	h2o = _del_h2o; 
	pres = _dpres; 
}


//----------Calculation methods----------

void Ambient::setDefaults(){
	
	var_map V;	// Empty varmap for setting defaults
	_wf_loaded = false;	//tracking flag 
	Create(V);
}

void Ambient::Create(var_map &V)
{
	_user_sun.resize_fill(2, 0.);
	_date_time.setDefaults();
	
	//Set defaults for the ambient object
	setVar("atm_coefs", _atm_coefs, V);		//Atmospheric attenuation coefficients for user-defined analysis
	setVar("atm_model", _atm_model, V, 0, "[0,3]");		//Atmospheric attenuation model {0=25km Barstow, 1 = 5km Barstow, 2 = user defined}
	setVar("date_mday", _date_time._mday, V, _date_time._mday, "[1,31]");		//Day of the month setting for date object
	setVar("date_month", _date_time._month, V, _date_time._month, "[1,12]");		//Month setting for date object
	setVar("date_wday", _date_time._wday, V, _date_time._wday, "[1,7]");		//Day of the week setting for date object
	setVar("date_yday", _date_time._yday, V, _date_time._yday, "[1,365]");		//Day of the year  setting for date object
	setVar("date_year", _date_time._year, V, _date_time._year, "[0.,2150.]");		//Year setting for date object
	setVar("del_h2o", _del_h2o, V, 20., "[0,9e9]");		//Atmospheric precipitable water depth for use in the Allen insolation model
	setVar("dni_layout", _dni_layout, V, 950., "[0,9999]");		//DNI to use during all layout calculations. CONSTANT model only.
	setVar("dpres", _dpres, V, 1., "[0,2]");		//Local ambient pressure relative to sea-level pressure
	setVar("elevation", _elevation, V, 588., "[-424.,8850.]");		//Plant mean elevation
	setVar("insol_type", _insol_type, V, -1, "[-1,4]");		//Model used to determine insolation as a function of time
	setVar("latitude", _latitude, V, 34.867, "[-90.,90.]");		//Plant latitude
	setVar("longitude", _longitude, V, -116.783, "[-180.,180.]");		//Plant longitude
	setVar("sun_azimuth", _azimuth, V, 0., "[-180.,180.]");		//User-set solar azimuth angle
	setVar("sun_csr", _sun_csr, V, 0.1, "[0,1)");		//Ratio of solar flux contained in the circumsolar ring over the solar disc flux
	setVar("sun_pos_map", _sun_pos, V);		//Map of sun positions to use for calculations
	setVar("sun_rad_limit", _sun_rad_limit, V, 4.65, "(0,1000]");		//Half-angle of sunshape size (4.65mrad for Pillbox, 2.73mrad for Gaussian)
	setVar("sun_type", _suntype, V, 1, "[0,3]");		//Sunshape model - {0=point sun, 1=limb darkened sun, 2=square wave sun, 3=user sun}
	setVar("sun_zenith", _zenith, V, 0.5, "[0.,90]");		//User-set solar zenith angle
	setVar("time_hour", _date_time._hour, V, _date_time._hour, "[0,24)");		//Hour of the day setting for date object
	setVar("time_min", _date_time._min, V, _date_time._min, "[0,60)");		//Minute of the hour setting for date object
	setVar("time_ms", _date_time._ms, V, _date_time._ms, "[0,1000)");		//Millisecond of the second setting for date object
	setVar("time_sec", _date_time._sec, V, _date_time._sec, "[0,60)");		//Second of the minute setting for date object
	setVar("time_zone", _time_zone, V, -8., "[-12,12]");		//Time zone
	setVar("user_sun", _user_sun, V);		//Array of intensity at various angles from the centroid of the sun
	setVar("weather_file", _weather_file, V, "CA Daggett.tm2");		//Weather file to use for analysis
	setVar("wf_data", _wf_data, V);		//Data entries in the weather file
	setVar("wf_type", _wf_type, V, "TM2");		//weather file type {TM2, TM3, SMW, EPW, USER}
	
	//Unit correction
	_latitude *= d2r;
	_longitude *= d2r;
	_azimuth *= d2r;
	_zenith *= d2r;

	//Apply the atmospheric attenuation coefficients if they haven't been set
	if(V.find("atm_coefs") == V.end()){
		if(_atm_model == 0){	//barstow 25km vis
			double atmc[] = {0.006789, 0.1046, -.017, 0.002845};
			setAtmCoefs( atmc, 4);
		}
		else if(_atm_model == 2){	//barstow 5km vis
			double atmc[] = {.01293, .2748,-.03394};
			setAtmCoefs( atmc, 3);
		}
	}
	
	//call SOLPOS to get default zenith/azimuth angles
	double sp[2];
	if(V.find("sun_zenith") == V.end() && V.find("sun_azimuth") == V.end()){
		calcSunPosition(sp);	//this updates _sun_pos so long as the user has not manually set the sun position
	
		//Also update _azimuth, _zenith
		_azimuth = sp[0];
		_zenith = sp[1];
	}
	
	//update the sun vector
	sunVector(_azimuth, _zenith);

}

void Ambient::Clean(){
	/* Clean out any key variables that shouldn't be available after a new field is created */
	_date_time.setDefaults();
	_wf_data.resizeAll(1,0.);
	_wf_loaded = false;
	_sun_vect.Set(0.,0.,1.);
}

void Ambient::setDateTime(double day_hour, double year_day, double year){
	_date_time.setZero();
	double min, sec;
	min = (day_hour - floor(day_hour))*60.;
	sec = (min - floor(min))*60.;
	_date_time.SetHour(int(floor(day_hour)));
	_date_time.SetMinute(int(min));
	_date_time.SetSecond(int(sec));
	_date_time.SetYearDay(int(year_day));
	_date_time.SetYear(int(year));
	int month, dom;
	_date_time.hours_to_date((year_day-1)*24.+day_hour, month, dom);
	_date_time.SetMonth(int(month));
	_date_time.SetMonthDay(int(dom));
}

void Ambient::setAtmCoefs(double coefs[], int n){
	_atm_coefs.resize(n);
	for(int i=0; i<n; i++){
		_atm_coefs.at(i) = coefs[i];
	}
}

Vect Ambient::sunVector(double azimuth, double zenith) {

	/* Calculate the unit vector for sun position (i,j,k) relative to 
	the plant location.

	This method is also called with the overloaded sunVector() that uses
	the local values of azimuth and zenith.
	*/

	double i, j, k; //i-> East, j->North, k->Vertical

	i = sin(azimuth)*sin(zenith);
	j = cos(azimuth)*sin(zenith);
	k = cos(zenith);

	_sun_vect.i = i;
	_sun_vect.j = j;
	_sun_vect.k = k;

	//Also set the local _sun_vect variable x.y.x coords to zero
	//_sun_vect.x = 0; _sun_vect.y = 0.; _sun_vect.z = 0.;
	
	return _sun_vect;	//return a copy
}

void Ambient::calcSunPosition(){
	double dum[2];
	calcSunPosition(dum, _latitude, _longitude, _time_zone, _date_time);
}

void Ambient::calcSunPosition(double sp[2]){
	//Use local info to calculate sun position
	calcSunPosition(sp, _latitude, _longitude, _time_zone, _date_time);
}

void Ambient::calcSunPosition(double sp[2], const DTobj &dt){
	//Use the local values for latitude, longitude, and time zone
	//Use an external source for the date-time information
	calcSunPosition(sp, _latitude, _longitude, _time_zone, dt);
}

void Ambient::calcSunPosition(double sp[2], double lat, double lon, double timezone, const DTobj &dt){
	/*
	Use SOLPOS to calculate the sun position.

	The required inputs for SOLPOS are:
		(via posdata)
		year, month, day of month, day of year, hour, minute, second,
		latitude, longitude, timezone, interval
	*/
	
	//Instantiate the solpos object
	struct posdata SP, *pdat;
	pdat = &SP;	//point to structure for convenience
	S_init(pdat);		//Initialize the values
	
	pdat->latitude = float(lat*r2d);		//[deg] {float} North is positive
	pdat->longitude = float(lon*r2d);		//[deg] {float} Degrees east. West is negative
	pdat->timezone = float(timezone);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
	pdat->year = dt._year;		//[year] {int} 4-digit year
	pdat->month = dt._month+1;	//[mo] {int} (1-12)
	pdat->day = dt._mday;		//[day] {int} Day of the month
	pdat->daynum = dt._yday;	//[day] {int} Day of the year
	pdat->hour = dt._hour;		//[hr] {int} 0-23
	pdat->minute = dt._min;	//[min] {int} 0-59
	pdat->second = dt._sec;	//[sec]	{int} 0-59
	pdat->interval = int(_sim_time_step);	//[sec] {int} Measurement interval, should correspond to the duration of the weather file time step. 
		//Note that the interval determines the time at which the sun position is calculated.
		//The sun position is calculated at (hour - interval/2.)


	long retcode = 0;		//Initialize with no errors
	retcode = S_solpos(pdat);	//Call the solar posotion algorithm
	S_decode(retcode, pdat);	//Check the return code

	//retrieve the output
	_sun_pos.resize(2);

	double azimuth, zenith;
	azimuth = SP.azim * d2r;
	zenith = SP.zenetr * d2r;

	//Calculate the sun vector
	sunVector(azimuth, zenith);
	
	//Assign to the local storage array
	_sun_pos[0] = azimuth; _sun_pos[1] = zenith;

	//Set up a new memory space to return the array
	sp[0] = azimuth; sp[1] = zenith;
	//Also set the azimuth and zenith angles of the sun
	_azimuth = azimuth; _zenith = zenith;

}

void Ambient::calcDaytimeHours(double hrs[2], double lat, double lon, double timezone, const DTobj &dt){
	/* Calculate the limiting hours during which the sun is above the horizon */
	struct posdata SP, *pdat;
	pdat = &SP;	//point to structure for convenience
	S_init(pdat);		//Initialize the values
	
	double r2d = 180./acos(-1);
	pdat->latitude = float(lat*r2d);		//[deg] {float} North is positive
	pdat->longitude = float(lon*r2d);		//[deg] {float} Degrees east. West is negative
	pdat->timezone = float(timezone);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
	pdat->year = dt._year;		//[year] {int} 4-digit year
	pdat->month = dt._month+1;	//[mo] {int} (1-12)
	pdat->day = dt._mday;		//[day] {int} Day of the month
	pdat->daynum = dt._yday;	//[day] {int} Day of the year
	pdat->hour = dt._hour;		//[hr] {int} 0-23
	pdat->minute = dt._min;	//[min] {int} 0-59
	pdat->second = dt._sec;	//[sec]	{int} 0-59
	pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


	long retcode = 0;		//Initialize with no errors
	retcode = S_solpos(pdat);	//Call the solar posotion algorithm
	//srss( pdat );
	S_decode(retcode, pdat);	//Check the return code
	hrs[0] = pdat->sretr/60.;
	hrs[1] = pdat->ssetr/60.;


}

void Ambient::readWeatherFile(){
	//Call the main method with the default values
	_wf_loaded = readWeatherFile(_wf_data, _weather_file, this);
}

bool Ambient::readWeatherFile(WeatherData &data, std::string &file_name, Ambient *Amb)
{
/*

	NOTE: This method does not currently implement psychrometric property algorithms or irradiance correction methods, 
	so data is used "as is" from the weather file. Many weather files do not provide wet bulb temperature directly and
	it must be calculated from dry bulb, relative humidity, and ambient pressure.

	This method takes as inputs:
	1) A pointer to the data map that will contain the weather file data. This map will have keys (uppercase) 
	   that correspond to the data label and an associated vector of the timestep data. The included data streams
	   are:

	   Key		|	Description						| Units
	   -----------------------------------------------------
	   DAY		|	Day of the month (1-31)			| days
	   MONTH	|	Month of the year (1-12)		| month
	   HOUR		|	Hour of the day (1-24)			| hr
	   DNI		|	Direct normal irradiation		| W/m2
	   T_DB		|	Dry bulb ambient temperature	| C
	   V WIND	|	Wind velocity					| m/s


	2) The file name
	*/	

	//Open the file
	weatherfile wf_reader;
	if(! wf_reader.open(file_name)) return false;	//Error
	//Read the header info
	if(Amb != (Ambient*)NULL){
		//If the ambient class reference is provided, set the local values in that class
		double d2r = acos(-1.)/180.;
		Amb->setPlantLocation(wf_reader.lat*d2r, wf_reader.lon*d2r, wf_reader.tz);
		Amb->setElevation(wf_reader.elev);
	}

	//Read in the weather data
	int nrec = wf_reader.nrecords;
	data.resizeAll(nrec);
	for(int i=0; i<nrec; i++){
		if(! wf_reader.read() ) return false; //Error
		data.Day.at(i) = (double)wf_reader.day;
		data.DNI.at(i) = (double)wf_reader.dn;
		data.Hour.at(i) = (double)wf_reader.hour;
		data.Month.at(i) = (double)wf_reader.month;
		data.Pres.at(i) = wf_reader.pres/1000.;	//bar
		data.T_db.at(i) = wf_reader.tdry;		//C
		data.V_wind.at(i) = wf_reader.wspd;	//m/s
		data.Step_weight.at(i) = 1.;	//default step
	}
	//wf_reader.close();
	return true;

}

double Ambient::calcAttenuation(double &len){
	/*
	Length in units of meters. 
	Atmospheric attenuation model set on Create

	Calculate atmospheric attenuation as a function of slant range. Model options are:
	0:	Barstow 25km (polynomials, DELSOL)
	1:  Barstow 5km visibility (polynomials, DELSOL)
	2:	User defined coefficients (polynomials)
	3:	Sengupta & Wagner model
	*/
	double att=0.0;
		
	if(_atm_model < 3){ //Barstow 25k or 5km visibility from DELSOL, user coefs
		double rkm = len*.001;
		int nc = (int)_atm_coefs.ncells();
		for(int i=0; i<nc; i++){ att += _atm_coefs.at(i)*pow(rkm, i); }
	}
	else if(_atm_model == 3){
		//Sengupta model
		//error("Attenuation Model", "The Sengupta/Wagner attenuation model is not yet implemented.");
		return 0.;
	}

	return 1.-att;

}

void Ambient::calcSpacedDaysHours(double lat, double lon, double tmz, int nday, double delta_hr, vector<vector<double> > &utime, vector<int> &uday){
	//Method taken from PTGen code (Wagner 2008 thesis)
	double pi = acos(-1.);
	uday.resize(nday);
	vector<int> 
		ntstep(nday),
		ntstep_day(nday);

	vector<double> 
		noons(nday),
		hours(nday);
	
	DateTime DT;
	int month, dom;

	for(int i=0; i<nday; i++){
		//Calculate the day number - The days are evenly distributed over the cosine wave of the year
		uday[i] = 355 - (int)floor(acos(-1.+2.*i/(float)(nday-1))/pi*(float)(355-172));
		
		DT.hours_to_date(uday[i]*24 +12., month, dom);
		DT.SetHour(12);
		DT.SetDate(2011, month, dom);
		DT.SetYearDay(uday[i]);
		double hrs[2];
		Ambient::calcDaytimeHours(hrs, lat, lon, tmz, DT);

		noons.at(i) = (hrs[0]+hrs[1])/2.;
		//shorten the time by 0.9 so we don't get stuff really close to the horizon
		ntstep[i] = (int)floor( (hrs[1]-noons.at(i))*.9/delta_hr );
		
		//For the calculated day, determine the solar declination angle and the number of daylight hours
		//delta[i] = asin(23.45*d2r*cos((float)(uday[i]-173)*d2r)); 
		//nhour[i] = (int)(floor((2./15.*acos(-tan(phi)*tan(delta[i]))*r2d)/2.));
		ntstep_day[i] = 2*ntstep[i]+1;
	}
	//Store the day and time in the utime array
	utime.clear();
	vector<double> utemp;
	int nflux_sim = 0;
	for(int i=0; i<nday; i++){
		utemp.clear();
		for(int j=0; j<ntstep_day[i]; j++){
			utemp.push_back( noons[i]+(-ntstep[i]+j)*delta_hr -12.);
			nflux_sim++;
		}
		utime.push_back(utemp);
	}
	
}

double Ambient::calcInsolation(double azimuth, double zenith) //calculate clear-sky radiation using one of the DELSOL models
{
	/* 
	Inputs:
	azimuth		|	solar azimuth (radians)
	zenith		|	solar zenith angle (radians)
	altitude	|	site elevation / altitude (kilometers)
	model		|	clear sky model { MEINEL, HOTTEL, CONSTANT, MOON, ALLEN }
	solcon		|	*required for CONSTANT*  specified DNI - (kW/m2)

	Delsol 7065-7082ish
	*/

	int doy = _date_time.GetDayOfYear();
	double S0 = 1.353*(1.+.0335*cos(2.*pi*(doy+10.)/365.));

	double szen = sin(zenith);
	double czen = cos(zenith);

	double save2 = 90. - atan2(szen, czen)*r2d;
    double save = 1.0/czen;
    if (save2 <= 30.) 
		save=save-41.972213*pow(save2,(-2.0936381-0.04117341*save2+0.000849854*pow(save2,2)));

	double ALT = _elevation / 1000.;
	double dni;

	switch (_insol_type)
	{
	case Ambient::MEINEL:
		dni = (1.-.14*ALT)*exp(-.357/pow(czen,.678))+.14*ALT;
		break;
	case Ambient::HOTTEL:
		dni = 0.4237-0.00821*pow(6.-ALT,2)+(0.5055+0.00595*pow(6.5-ALT,2))*exp(-(0.2711+0.01858*pow(2.5-ALT,2))/(czen+.00001));
		break;
	case Ambient::CONSTANT:
		dni = _dni_layout / (S0 * 1000.);
		break;
	case Ambient::MOON:
		dni = 1.0-0.263*((_del_h2o+2.72)/(_del_h2o+5.0))*pow((save*_dpres),(0.367*((_del_h2o+11.53)/(_del_h2o+7.88))) );
		break;
	case Ambient::ALLEN:
		dni = 0.183*exp(-save*_dpres/0.48)+0.715*exp(-save*_dpres/4.15)+.102;
		break;
	default:
		throw spexception("The specified clear sky DNI model is not available.");
		break;
	}
	return dni * S0 * 1000.;
	
}