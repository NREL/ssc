#include "lib_pvmodel.h"
#include <math.h>
#include <limits>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif


pvinput_t::pvinput_t()
{
	Ibeam = Idiff = Ignd = Tdry 
		= Zenith = IncAng = Elev 
		= Tilt = std::numeric_limits<double>::quiet_NaN();
}


pvinput_t::pvinput_t( double ib, double id, double ig, 
		double td, double ws,
		double zen, double inc, 
		double elv, double tlt)
{


	Ibeam = ib;
	Idiff = id;
	Ignd = ig;
	Tdry = td;
	Wspd = ws;
	Zenith = zen;
	IncAng = inc;
	Elev = elv;
	Tilt = tlt;
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
