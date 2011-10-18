#ifndef cec6par_h
#define cec6par_h

#include "lib_pvmodel.h"

/*
   Implementation of CEC 5 (6) parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
*/

class mcphys_celltemp_t : public pvcelltemp_t
{
public:

	double Pmp_ref;

	enum{ RACK, FLUSH, INTEGRATED, GAP };
	int mc;

	enum{ NOIMPEDE, VERTSUPP, HORIZSUPP };
	int orient; // 0=do not impede flow, 1=vertical supports, 2=horizontal supports

	// array dimensions
	int nrows;
	int ncols;

	double module_width;
	double module_length;
	double W_gap; 

	mcphys_celltemp_t( );
	virtual bool operator() ( pvinput_t &input, pvpower_t &pwrfunc, double *Tc );
};

class noct_celltemp_t : public pvcelltemp_t
{
public:
	double Area;
	double Vmp;
	double Imp;
	double Tnoct;

	noct_celltemp_t( );
	virtual bool operator() ( pvinput_t &input, pvpower_t &pwrfunc, double *Tc );
};

class cec6par_power_t : public pvpower_t
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

	cec6par_power_t();
	virtual bool operator() ( pvinput_t &input, double Tc, double opvoltage,
		double *P, double *V, double *I,
		double *Eff, double *Voc, double *Isc );
};

#endif

