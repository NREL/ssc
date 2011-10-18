#ifndef __pvmodulemodel_h
#define __pvmodulemodel_h

#include <string>

class pvcelltemp_t;
class pvpower_t;

class pvinput_t
{
public:
	pvinput_t( double ib, double id, double ig, double ta, double ws, double zen, double inc, double elv, double tlt );

	/* angles are in degrees */

	double Ibeam;
	double Idiff;
	double Ignd;
	double Tamb;
	double Wspd;
	double Zenith;
	double IncAng;
	double Elev;
	double Tilt;
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
