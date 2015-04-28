#ifndef __irradproc_h
#define __irradproc_h

/* aug2011 - apd
	solar position and radiation processing split out from pvwatts.
	added isotropic sky model and hdkr model for diffuse on a tilted surface
	*/

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5]);
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );
void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );




class irrad
{
private:
	int year, month, day, hour;
	double minute, delt;

	double lat, lon, tz;
	int radmode, skymodel, track;
	double gh, dn, df, alb, diff_vf;
	double tilt, sazm, rlim, gcr;
	bool en_backtrack;
	double sun[9], angle[5], poa[3], diffc[3];
	int tms[3];
	double ghi;

	

public:

	irrad();
	int check();

	void set_time( int year, int month, int day, int hour, double minute, double delt_hr );
	void set_location( double lat, double lon, double tz );
	//skymodel: 0 is isotropic, 1 is hdkr, 2 is perez
	void set_sky_model( int skymodel, double albedo, double diff_view_factor=1.0 );
	void set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr );
	void set_beam_diffuse( double beam, double diffuse );
	void set_global_beam( double global, double beam );
	void set_global_diffuse(double global, double diffuse);

	int calc();
	
	void get_sun( double *solazi,
		double *solzen,
		double *solelv,
		double *soldec,
		double *sunrise,
		double *sunset,
		int *sunup,
		double *eccfac,
		double *tst,
		double *hextra );
	void get_angles( double *aoi,
		double *surftilt,
		double *surfazi,
		double *axisrot,
		double *btdiff );
	void get_poa( double *beam, double *skydiff, double *gnddiff,
		double *isotrop, double *circum, double *horizon );
	double get_ghi();
};




double shade_fraction_1x( double solazi, double solzen, 
						 double axis_tilt, double axis_azimuth, 
						 double gcr, double rotation );

double backtrack( double solazi, double solzen, 
				 double axis_tilt, double axis_azimuth, 
				 double rotlim, double gcr, double rotation_ideal);

#endif
