#include "lib_pvmodel.h"
#include <math.h>
#include <limits>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif


pvinput_t::pvinput_t()
{
	Ibeam = Idiff = Ignd = Tdry = Tdew = Wspd = Wdir = Patm
		= Zenith = IncAng = Elev 
		= Tilt = Azimuth = HourOfDay = std::numeric_limits<double>::quiet_NaN();
}


pvinput_t::pvinput_t( double ib, double id, double ig, 
		double ta, double td, double ws, double wd, double patm,
		double zen, double inc, 
		double elv, double tlt, double azi,
		double hrday )
{
	Ibeam = ib;
	Idiff = id;
	Ignd = ig;
	Tdry = ta;
	Tdew = td;
	Wspd = ws;
	Wdir = wd;
	Patm = patm;
	Zenith = zen;
	IncAng = inc;
	Elev = elv;
	Tilt = tlt;
	Azimuth = azi;
	HourOfDay = hrday;
}

std::string pvcelltemp_t::error()
{
	return m_err;
}

pvoutput_t::pvoutput_t()
{
	Power = Voltage = Current = Efficiency
		= Voc_oper = Isc_oper = std::numeric_limits<double>::quiet_NaN();
}


std::string pvmodule_t::error()
{
	return m_err;
}
