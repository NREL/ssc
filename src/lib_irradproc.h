#ifndef __irradproc_h
#define __irradproc_h

/* aug2011 - apd
	solar position and radiation processing split out from pvwatts.
	added isotropic sky model and hdkr model for diffuse on a tilted surface
	*/

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm,double angle[3]);
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );
void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

#endif
