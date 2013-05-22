#ifndef __lib_pvinv_h
#define __lib_pvinv_h

#include <vector>
/*
	Implementation of inverter partload curve with linear interpolation
*/

class partload_inverter_t
{
public:
	partload_inverter_t( );

	double Paco;    /* Maximum AC power rating, upper limit value  (Wac) */
	double Pdco;    /* DC power level at which Paco is achieved (Wdc) */
	std::vector<double> Partload; /* Array of partload values (Pdc/Paco) for linear interpolation */
	std::vector<double> Efficiency; /* Array of efficiencies corresponding to partload values */

	bool acpower(	/* inputs */
		double Pdc,     /* Input power to inverter (Wdc) */

		/* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Eff	    /* Conversion efficiency (0..1) */
		);

} ;

#endif
