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
		= Voc_oper = Isc_oper = CellTemp = std::numeric_limits<double>::quiet_NaN();
}


pvoutput_t::pvoutput_t( double p, double v,
		double c, double e, 
		double voc, double isc, double t )
{
	Power = p;
	Voltage = v;
	Current = c;
	Efficiency = e;
	Voc_oper = voc;
	Isc_oper = isc;
	CellTemp = t;
}


std::string pvmodule_t::error()
{
	return m_err;
}

spe_module_t::spe_module_t( )
{
	Area = 0;
	Gamma = 0;
	Reference = 0;
	fd = 1;
	for (int i=0;i<5;i++)
		Eff[i] = Rad[i] = 0;
}

	
double spe_module_t::eff_interpolate( double irrad, double rad[5], double eff[5] )
{
	if ( irrad < rad[0] )
		return eff[0];
	else if ( irrad > rad[4] )
		return eff[4];

	int i = 1;
	for ( i=1;i<5;i++ )
		if ( irrad < rad[i] ) break;
      
	int i1 = i-1;

	double wx=(irrad-rad[i1])/(rad[i]-rad[i1]);
	return (1-wx)*eff[i1]+wx*eff[i];
}

bool spe_module_t::operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output)
{
	double idiff = fd*(input.Idiff + input.Ignd);
	double dceff = eff_interpolate( input.Ibeam + idiff, Rad, Eff );
	double dcpwr = dceff*(input.Ibeam+idiff)*Area;	
	dcpwr += dcpwr*(Gamma/100.0)*(TcellC - 25.0);
	if (dcpwr < 0) dcpwr = 0;

	output.CellTemp = TcellC;
	output.Efficiency = dceff;	
	output.Power = dcpwr;
	output.Voltage = VmpRef();
	output.Current = output.Power / output.Voltage;
	output.Isc_oper = IscRef();
	output.Voc_oper = VocRef();
	return true;
}
