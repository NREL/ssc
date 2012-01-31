#ifndef __pvmodulemodel_h
#define __pvmodulemodel_h

#include <string>

class pvcelltemp_t;
class pvpower_t;

class pvinput_t
{
public:
	pvinput_t();
	pvinput_t( double ib, double id, double ig, 
		double ta, double td, double ws, double wd, double patm,
		double zen, double inc, 
		double elv, double tlt, double azi);

	double Ibeam; // beam irradiance, W/m2
	double Idiff; // sky diffuse irradiance, W/m2
	double Ignd; // ground reflected irradiance, W/m2
	double Tdry; // dry bulb temp, C
	double Tdew; // dew point temp, C
	double Wspd; // wind speed, m/s
	double Wdir; // wind direction, deg +from north
	double Patm; // atmospheric pressure, millibar
	double Zenith; // zenith angle, deg
	double IncAng; // incidence angle on surface, deg
	double Elev; // site elevation, m
	double Tilt; // surface tilt angle, deg +from horizontal
	double Azimuth; // surface azimuth angle, deg +from north (E=90,S=180)
	double HourOfDay; // hour of the day 0=12am, 23=11pm
};

class pvoutput_t
{
public:
	pvoutput_t();

	double Power; // output power, Watts
	double Voltage; // operating voltage, V
	double Current; // operating current, A
	double Efficiency; // operating efficiency, fraction (0..1)
	double Voc_oper; // open circuit voltage at operating condition, V
	double Isc_oper; // short circuit current at operating condition, A
	double CellTemp; // cell temperature, 'C
};

class pvmodule_t; // forward decl

class pvcelltemp_t
{
protected:
	std::string m_err;
public:
	
	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell ) = 0;
	std::string error();
};

class pvmodule_t
{
protected:
	std::string m_err;
public:

	virtual double AreaRef() = 0;
	virtual double VmpRef() = 0;
	virtual double ImpRef() = 0;
	virtual double VocRef() = 0;
	virtual double IscRef() = 0;

	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output ) = 0;
	std::string error();
};


#endif
