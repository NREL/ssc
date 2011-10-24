#include "lib_pvmodel.h"
#include <math.h>
#include <limits>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif


pvinput_t::pvinput_t( double ib, double id, double ig, 
		double ta, double tw, double td,
		double ws, double wd, double atm,
		double zen, double inc, 
		double elv, double tlt, double azi,
		double todh  )
{


	Ibeam = ib;
	Idiff = id;
	Ignd = ig;
	Tdry = ta;
	Twet = tw;
	Tdew = td;
	Wspd = ws;
	Wdir = wd;
	Zenith = zen;
	IncAng = inc;
	Elev = elv;
	Tilt = tlt;
	Azimuth = azi;
	Patm = atm;
	TimeOfDayHr = todh;
}

std::string pvcelltemp_t::error()
{
	return m_err;
}

std::string pvpower_t::error()
{
	return m_err;
}

bool pvmodule_function( pvinput_t &input, pvcelltemp_t &tcfunc, pvpower_t &pwrfunc, double opvol,
	double *P, double *I, double *V, double *Voc, double *Isc, double *Eff, double *Tcell, std::string *err )
{
	if (! tcfunc( input, pwrfunc, opvol, Tcell ) )
	{
		if (err) *err = tcfunc.error();
		return false;
	}

	if (! pwrfunc( input, *Tcell, opvol, P, V, I, Eff, Voc, Isc ))
	{
		if (err) *err = pwrfunc.error();
		return false;
	}

	if (err) err->clear();

	return true;
}
