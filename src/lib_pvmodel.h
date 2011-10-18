#ifndef __pvmodulemodel_h
#define __pvmodulemodel_h

#include <string>

class pvcelltemp_t;
class pvpower_t;

class pvinput_t
{
public:
	pvinput_t( double ib, double id, double ig, 
		double ta, double tw, double td,
		double ws, double wd, double atm,
		double zen, double inc, 
		double elv, double tlt, double azi,
		double todh );

	double Ibeam; // beam irradiance, W/m2
	double Idiff; // sky diffuse irradiance, W/m2
	double Ignd; // ground reflected irradiance, W/m2
	double Tdry; // dry bulb temp, C
	double Twet; // wet bulb temp, C
	double Tdew; // dew point temp, C
	double Wspd; // wind speed, m/s
	double Wdir; // wind direction, degrees (0=N,90=E)
	double Zenith; // zenith angle, deg
	double IncAng; // incidence angle on surface, deg
	double Elev; // site elevation, m
	double Tilt; // surface tilt angle, deg
	double Azimuth; // surface azimuth angle, deg
	double Patm; // atmospheric pressure, millibar 
	double TimeOfDayHr; // hours since midnight
};

class pvcelltemp_t
{
protected:
	std::string m_err;
public:
	virtual bool operator() ( pvinput_t &input, pvpower_t &pwrfunc, double *Tc ) = 0;
	std::string error();
};

class pvpower_t
{
protected:
	std::string m_err;
public:

	virtual bool operator() ( pvinput_t &input, double Tc, double opvoltage, /* by default, for mppt, use negative opvoltage */
		double *Power, double *Voltage, double *Current,
		double *Eff, double *OpVoc, double *OpIsc ) = 0;
	std::string error();
};


bool pvmodule_function( pvinput_t &input, pvcelltemp_t &tcfunc, pvpower_t &pwrfunc, double opvol,
	double *P, double *V, double *I, double *Eff, double *Voc, double *Isc, double *Tcell );


#endif
