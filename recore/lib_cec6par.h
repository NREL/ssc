#ifndef cec6par_h
#define cec6par_h

#include "lib_pvmodel.h"

/*
   Implementation of CEC 5 (6) parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
*/

class cec6par_module_t : public pvmodule_t
{
public:	
	double Area;
	double Vmp;
	double Imp;
	double Voc;
	double Isc;
	double alpha_isc;
	double beta_voc;
	double Tnoct;
	double a;
	double Il;
	double Io;
	double Rs;
	double Rsh;
	double Adj;
	double standoff_tnoct_adj;
	double ffv_wind;

	cec6par_module_t();

	virtual double VmpRef() { return Vmp; }
	virtual double ImpRef() { return Imp; }
	virtual double VocRef() { return Voc; }
	virtual double IscRef() { return Isc; }
	virtual bool operator() ( pvinput_t &input, double opvoltage, pvoutput_t &output );
};

#endif

