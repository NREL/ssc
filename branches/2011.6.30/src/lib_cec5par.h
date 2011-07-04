#ifndef __lib_cec5par_h
#define __lib_cec5par_h

/*
   Implementation of CEC 5 parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
   
   Includes implementation of advanced temperature model by
   Ty Neises, Masters Thesis 2011
   http://sel.me.wisc.edu/publications/theses/neises11.zip
*/

struct cec5par_module_t {

	double Area;
	double Isc0, Voc0;
	double Tc0, G0;
	double Imp0, Vmp0;
	double mIsc, mVoc;
	double TcNOCT;
	double A0, IL0, IO0;
	double Rs;
	double Rsh0;
	double Adjust;
	
	
	double TaNOCT;
	double G_NOCT;
	double TauAlpha;
	double Eg0;
	double KL; // proportionality factor * cover thickness, generally 0.008 for a 2 mm cover
	
	/* advanced temp model inputs */
		double Width, Length;
		int MC, MSO, HTD;
		double Wgap;
		int NRows, NCols;
		double Derate;
};

/*
	returns 0 on success.
   -1 indicates temp model failed to converge
*/
int cec5par_module(
	/* parameters */
	cec5par_module_t *pModule,
	
	/* inputs */
	double G_beam,/* beam irradiance on tilted surface, W/m2 */
	double G_sky, /* sky diffuse irradiance on tilted surface, W/m2 */
	double G_gnd, /* ground diffuse irradiance on tilted surface, W/m2 */
	double T_amb, /* ambient temperature, C */
	double tilt, /* tilt (slope) of array, deg - used to calculate effective incidence angles for sky and ground diffuse */
	double theta, /* Incidence angle on tilted surface, deg */
	double theta_z, /* zenith angle, deg*/
	double W_spd, /* wind speed, m/s */
	
	/* outputs */
	double *Pmp,
	double *Vmp, double *Imp,
	double *Voc, double *Isc,
	double *Eff, double *Tcell, 
	double *Geff
	);

#endif

