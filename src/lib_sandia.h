#ifndef __lib_sandia_h
#define __lib_sandia_h

#include "lib_pvmodel.h"


/*
	Implementation of report SAND2004-3535
	Photovoltaic Array Performance Model
	King, Boyson, Kratochvil

	http://photovoltaics.sandia.gov/docs/PDF/King%20SAND.pdf
*/

class sandia_celltemp_t : public pvcelltemp_t
{
	double a;
	double b;
	double DT0;
	double fd;

	sandia_celltemp_t( );
	virtual bool operator() ( pvinput_t &input, pvpower_t &pwrfunc, double opvol, double *Tc );
};

class sandia_power_t : public pvpower_t
{
public:
	
	double a, b, DT0;
	double A0, A1, A2, A3, A4;
	double B0, B1, B2, B3, B4, B5;
	double C0, C1, C2, C3, C4, C5, C6, C7;
	double Isc0, aIsc;
	double Imp0, aImp;
	double Voc0, BVoc0, mBVoc;
	double Vmp0, BVmp0, mBVmp;
	double Ix0, Ixx0;
	double fd, DiodeFactor, NcellSer;
	double Area;

	sandia_power_t( );			
	virtual bool operator() ( pvinput_t &input, double Tc, double opvoltage, /* by default, mppt, send MPPT_VOLTAGE */
		double *Power, double *Voltage, double *Current,
		double *Eff, double *OpVoc, double *OpIsc );
};



/*
	Implementation of report SAND2007-5036
	Performance Model for Grid-Connected Photovoltaic Inverters
	King, Gonzalez, Galbraith, Boyson

	http://photovoltaics.sandia.gov/Pubs_2010/old/075036.pdf
*/

class sandia_inverter_t
{
public:
	sandia_inverter_t( );

	double Paco;    /* Maximum AC power rating, upper limit value  (Wac) */
	double Pdco;    /* DC power level at which Paco is achieved (Wdc) */
	double Vdco;    /* DC voltage level at which Paco is achieved (Vdc) */
	double Pso;     /* DC power require to start inversion process, or self-consumption by inverter (Wdc) */
	double Pntare;  /* AC power consumed by inverter at night as parasitic load (Wac) */
	double C0;      /* (1/W, empirical, default 0) Defines parabolic curvature of relationship between ac power and dc power at reference conditions */
	double C1;      /* (1/V, empirical, default 0) Parameter allowing Pdco to vary linearly with dc voltage input */
	double C2;      /* (1/V, empirical, default 0) Parameter allowing Pso to vary linearly with dc voltage input */
	double C3;      /* (1/V, empirical, default 0) Parameter allowing C0 to vary linearly with dc voltage input */

	bool acpower(	/* inputs */
		double Pdc,     /* Input power to inverter (Wdc) */
		double Vdc,     /* Voltage input to inverter (Vdc) */

		/* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Ppar,   /* AC parasitic power consumption (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Eff	    /* Conversion efficiency (0..1) */
		);
} ;

#endif
