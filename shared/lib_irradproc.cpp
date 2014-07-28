
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include <limits>

#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif


static const int __nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

static int julian(int yr,int month,int day)    /* Calculates julian day of year */
{
	int i=1,jday=0,k;

	if( yr%4 == 0 )                      /* For leap years */
		k = 1;
	else
		k = 0;

	while( i < month )
		{
		jday = jday + __nday[i-1];
		i++;
		}
	if( month > 2 )
		jday = jday + k + day;
	else
		jday = jday + day;
	return(jday);
}

static int day_of_year( int month, int day_of_month ) /* returns 1-365 */
{
	int i=1,iday=0;

	while ( i < month )
		iday += __nday[i++ - 1];

	return iday + day_of_month;
}

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9])
{
/* This function is based on a paper by Michalsky published in Solar Energy
	Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
	time and location passed to the function based on the Astronomical
	Almanac's Algorithm for the period 1950-2050. For data averaged over an
	interval, the appropriate time passed is the midpoint of the interval.
	(Example: For hourly data averaged from 10 to 11, the time passed to the
	function should be 10 hours and 30 minutes). The exception is when the time
	interval includes a sunrise or sunset. For these intervals, the appropriate
	time should be the midpoint of the portion of the interval when the sun is
	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
	sunrise time of 7:30, the time passed to the function should be 7 hours and
	and 45 minutes).

	Revised 5/15/98. Replaced algorithm for solar azimuth with one by Iqbal
	so latitudes below the equator are correctly handled. Also put in checks
	to allow an elevation of 90 degrees without crashing the program and prevented
	elevation from exceeding 90 degrees after refraction correction.

	This function calls the function julian to get the julian day of year.

	List of Parameters Passed to Function:
	year   = year (e.g. 1986)
	month  = month of year (e.g. 1=Jan)
	day    = day of month
	hour   = hour of day, local standard time, (1-24, or 0-23)
	minute = minutes past the hour, local standard time
	lat    = latitude in degrees, north positive
	lng    = longitude in degrees, east positive
	tz     = time zone, west longitudes negative

	sunn[]  = array of elements to return sun parameters to calling function
	sunn[0] = azm = sun azimuth in radians, measured east from north, 0 to 2*pi
	sunn[1] = 0.5*pi - elv = sun zenith in radians, 0 to pi
	sunn[2] = elv = sun elevation in radians, -pi/2 to pi/2
	sunn[3] = dec = sun declination in radians
	sunn[4] = sunrise in local standard time (hrs), not corrected for refraction
	sunn[5] = sunset in local standard time (hrs), not corrected for refraction
	sunn[6] = Eo = eccentricity correction factor
	sunn[7] = tst = true solar time (hrs)               
	sunn[8] = hextra = extraterrestrial solar irradiance on horizontal at particular time (W/m2)  */

	int jday,delta,leap;                           /* Local variables */
	double zulu,jd,time,mnlong,mnanom,
			eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
			E,ws,sunrise,sunset,Eo,tst;
	double arg,hextra,Gon,zen;

	jday = julian(year,month,day);       /* Get julian day of year */
	zulu = hour + minute/60.0 - tz;      /* Convert local time to zulu time */
	if( zulu < 0.0 )                     /* Force time between 0-24 hrs */
		{                                 /* Adjust julian day if needed */
		zulu = zulu + 24.0;
		jday = jday - 1;
		}
	else if( zulu > 24.0 )
		{
		zulu = zulu - 24.0;
		jday = jday + 1;
		}
	delta = year - 1949;
	leap = delta/4;
	jd = 32916.5 + delta*365 + leap + jday + zulu/24.0;
	time = jd - 51545.0;     /* Time in days referenced from noon 1 Jan 2000 */

	mnlong = 280.46 + 0.9856474*time;
	mnlong = fmod((double)mnlong,360.0);         /* Finds doubleing point remainder */
	if( mnlong < 0.0 )
		mnlong = mnlong + 360.0;          /* Mean longitude between 0-360 deg */

	mnanom = 357.528 + 0.9856003*time;
	mnanom = fmod((double)mnanom,360.0);
	if( mnanom < 0.0 )
		mnanom = mnanom + 360.0;
	mnanom = mnanom*DTOR;             /* Mean anomaly between 0-2pi radians */

	eclong = mnlong + 1.915*sin(mnanom) + 0.020*sin(2.0*mnanom);
	eclong = fmod((double)eclong,360.0);
	if( eclong < 0.0 )
		eclong = eclong + 360.0;
	eclong = eclong*DTOR;       /* Ecliptic longitude between 0-2pi radians */

	oblqec = ( 23.439 - 0.0000004*time )*DTOR;   /* Obliquity of ecliptic in radians */
	num = cos(oblqec)*sin(eclong);
	den = cos(eclong);
	ra  = atan(num/den);                         /* Right ascension in radians */
	if( den < 0.0 )
		ra = ra + M_PI;
	else if( num < 0.0 )
		ra = ra + 2.0*M_PI;

	dec = asin( sin(oblqec)*sin(eclong) );       /* Declination in radians */

	gmst = 6.697375 + 0.0657098242*time + zulu;
	gmst = fmod((double)gmst,24.0);
	if( gmst < 0.0 )
		gmst = gmst + 24.0;         /* Greenwich mean sidereal time in hours */

	lmst = gmst + lng/15.0;
	lmst = fmod((double)lmst,24.0);
	if( lmst < 0.0 )
		lmst = lmst + 24.0;
	lmst = lmst*15.0*DTOR;         /* Local mean sidereal time in radians */

	ha = lmst - ra;
	if( ha < -M_PI )
		ha = ha + 2*M_PI;
	else if( ha > M_PI )
		ha = ha - 2*M_PI;             /* Hour angle in radians between -pi and pi */

	lat = lat*DTOR;                /* Change latitude to radians */

	arg = sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha);  /* For elevation in radians */
	if( arg > 1.0 )
		elv = M_PI/2.0;
	else if( arg < -1.0 )
		elv = -M_PI/2.0;
	else
		elv = asin(arg);

	if( cos(elv) == 0.0 )
		{
		azm = M_PI;         /* Assign azimuth = 180 deg if elv = 90 or -90 */
		}
	else
		{                 /* For solar azimuth in radians per Iqbal */
		arg = ((sin(elv)*sin(lat)-sin(dec))/(cos(elv)*cos(lat))); /* for azimuth */
		if( arg > 1.0 )
			azm = 0.0;              /* Azimuth(radians)*/
		else if( arg < -1.0 )
			azm = M_PI;
		else
			azm = acos(arg);

		if( ( ha <= 0.0 && ha >= -M_PI) || ha >= M_PI )
			azm = M_PI - azm;
		else
			azm = M_PI + azm;
		}

	elv = elv/DTOR;          /* Change to degrees for atmospheric correction */
	if( elv > -0.56 )
		refrac = 3.51561*( 0.1594 + 0.0196*elv + 0.00002*elv*elv )/( 1.0 + 0.505*elv + 0.0845*elv*elv );
	else
		refrac = 0.56;
	if( elv + refrac > 90.0 )
		elv = 90.0*DTOR;
	else
		elv = ( elv + refrac )*DTOR ; /* Atmospheric corrected elevation(radians) */

	E = ( mnlong - ra/DTOR )/15.0;       /* Equation of time in hours */
	if( E < - 0.33 )   /* Adjust for error occuring if mnlong and ra are in quadrants I and IV */
		E = E + 24.0;
	else if( E > 0.33 )
		E = E - 24.0;

	arg = -tan(lat)*tan(dec);
	if( arg >= 1.0 )
		ws = 0.0;                         /* No sunrise, continuous nights */
	else if( arg <= -1.0 )
		ws = M_PI;                          /* No sunset, continuous days */
	else
		ws = acos(arg);                   /* Sunrise hour angle in radians */

					/* Sunrise and sunset in local standard time */
	sunrise = 12.0 - (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;
	sunset  = 12.0 + (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;

	Eo = 1.00014 - 0.01671*cos(mnanom) - 0.00014*cos(2.0*mnanom);  /* Earth-sun distance (AU) */
	Eo = 1.0/(Eo*Eo);                    /* Eccentricity correction factor */

	tst = hour + minute/60.0 + (lng/15.0 - tz) + E;  /* True solar time (hr) */
	
	/* 25aug2011 apd: addition of calculation of horizontal extraterrestrial irradiance */
	zen = 0.5*M_PI - elv;
	Gon = 1367*(1+0.033*cos( 360.0/365.0*day_of_year(month,day)*M_PI/180 )); /* D&B eq 1.4.1a, using solar constant=1367 W/m2 */
	if (zen > 0 && zen < M_PI/2) /* if sun is up */
		hextra = Gon*cos(zen); /* elevation is incidence angle (zen=90-elv) with horizontal */
	else if (zen == 0)
		hextra = Gon;
	else
		hextra = 0.0;

	sunn[0] = azm;                        /* Variables returned in array sunn[] */
	sunn[1] = zen;               /*  Zenith */
	sunn[2] = elv;
	sunn[3] = dec;
	sunn[4] = sunrise;
	sunn[5] = sunset;
	sunn[6] = Eo;
	sunn[7] = tst;
	sunn[8] = hextra;
}


void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5])
{
/* This function calculates the incident angle of direct beam radiation to a
	surface for a given sun position, latitude, and surface orientation. The
	modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
	Azimuth angles are for N=0 or 2pi, E=pi/2, S=pi, and W=3pi/2.  8/13/98

	List of Parameters Passed to Function:
	mode         = 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking, 3 for azimuth-axis tracking
	tilt         = tilt angle of surface from horizontal in degrees (mode 0),
				   or tilt angle of tracker axis from horizontal in degrees (mode 1),
				   MUST BE FROM 0 to 90 degrees.
	sazm         = surface azimuth in degrees of collector (mode 0), or surface
				   azimuth of tracker axis (mode 1) with axis azimuth directed from
				   raised to lowered end of axis if axis tilted.
	rlim         = plus or minus rotation in degrees permitted by physical constraints
			      	of tracker, range is 0 to 180 degrees.
	zen          = sun zenith in radians, MUST BE LESS THAN PI/2
	azm          = sun azimuth in radians, measured east from north
	en_backtrack = enable backtracking, using Ground coverage ratio ( below )
	gcr          = ground coverage ratio ( used for backtracking )

	Parameters Returned:
	angle[]  = array of elements to return angles to calling function
	angle[0] = inc  = incident angle in radians
	angle[1] = tilt = tilt angle of surface from horizontal in radians
	angle[2] = sazm = surface azimuth in radians, measured east from north
	angle[3] = rot = tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
	angle[4] = btdiff = (rot - ideal_rot) will be zero except in case of backtracking for 1 axis tracking
	*/
	/* Local variables: rot is the angle that the collector is rotated about the
	axis when viewed from the raised end of the 1-axis tracker. If rotated
	counter clockwise the angle is negative. Range is -180 to +180 degrees.
	When xsazm = azm : rot = 0, tilt = xtilt, and sazm = xsazm = azm  */

	double arg,inc=0,xsazm,xtilt,rot,btdiff=0;

	switch ( mode )
		{
		case 0:              /* Fixed-Tilt, */
		case 3:              /* or Azimuth Axis*/
			tilt = tilt*DTOR;    /* Change tilt and surface azimuth to radians */
			sazm = (mode==0) ? sazm*DTOR : azm; /* either fixed surface azimuth or solar azimuth */
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
			rot = 0;
			if( arg < -1.0 )
				inc = M_PI;
			else if( arg > 1.0  )
				inc = 0.0;
			else
				inc = acos(arg);
			break;
		case 1:                 /* One-Axis Tracking */
			xtilt = tilt*DTOR;   /* Change axis tilt, surface azimuth, and rotation limit to radians */
			xsazm = sazm*DTOR;
			rlim  = rlim*DTOR;
									/* Find rotation angle of axis for peak tracking */
			if( fabs( cos(xtilt) ) < 0.001745 )    /* 89.9 to 90.1 degrees */
				{          /* For vertical axis only */
				if( xsazm <= M_PI )
					{
					if( azm <= xsazm + M_PI )
						rot = azm - xsazm;
					else
						rot = azm - xsazm - 2.0*M_PI;
					}
				else        /* For xsazm > pi */
					{
					if( azm >= xsazm - M_PI )
						rot = azm - xsazm;
					else
						rot = azm - xsazm + 2.0*M_PI;
					}
				}
			else          /* For other than vertical axis */
				{
				arg = sin(zen)*sin(azm-xsazm)/
						( sin(zen)*cos(azm-xsazm)*sin(xtilt) + cos(zen)*cos(xtilt) );
				if( arg < -99999.9 )
					rot = -M_PI/2.0;
				else if( arg > 99999.9 )
					rot = M_PI/2.0;
				else
					rot = atan(arg);
								/* Put rot in II or III quadrant if needed */
				if( xsazm <= M_PI )
					{
					if( azm > xsazm && azm <= xsazm + M_PI )
						{     /* Ensure positive rotation */
						if( rot < 0.0 )
							rot = M_PI + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
					else
						{     /* Ensure negative rotation  */
						if( rot > 0.0 )
							rot = rot - M_PI;   /* Put in III quadrant: -90 to -180 deg */
						}
					}
				else        /* For xsazm > pi */
					{
					if( azm < xsazm && azm >= xsazm - M_PI )
						{     /* Ensure negative rotation  */
						if( rot > 0.0 )
							rot = rot - M_PI;   /* Put in III quadrant: -90 to -180 deg */
						}
					else
						{     /* Ensure positive rotation */
						if( rot < 0.0 )
							rot = M_PI + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
					}
				}
	  /*    printf("rot=%6.1f azm=%6.1f xsazm=%6.1f xtilt=%6.1f zen=%6.1f\n",rot/DTOR,azm/DTOR,xsazm/DTOR,xtilt/DTOR,zen/DTOR);  */

			if( rot < -rlim ) /* Do not let rotation exceed physical constraints */
				rot = -rlim;
			else if( rot > rlim )
				rot = rlim;

			// apd: added 21jan2012 to enable backtracking for 1 axis arrays using 3D iterative method
			// coded originally by intern M.Kasberg summer 2011
			if ( en_backtrack )
			{
				// find backtracking rotation angle
				double backrot = backtrack( azm*180/M_PI, zen*180/M_PI, // solar azimuth, zenith (deg)
					tilt, sazm, // axis tilt, axis azimuth (deg)
					rlim*180/M_PI, gcr, // rotation limit, GCR
					rot*180/M_PI ); // ideal rotation angle

				btdiff = backrot - rot*180/M_PI; // log the difference (degrees)
				btdiff *= M_PI/180; // convert output to radians
				rot = backrot * M_PI/180; // convert backtracked rotation angle to radians
			}


			/* Find tilt angle for the tracking surface */
			arg = cos(xtilt)*cos(rot);
			if( arg < -1.0 )
				tilt = M_PI;
			else if( arg > 1.0  )
				tilt = 0.0;
			else
				tilt = acos(arg);
									/* Find surface azimuth for the tracking surface */
			if( tilt == 0.0 )
				sazm = M_PI;     /* Assign any value if tilt is zero */
			else
				{
				arg = sin(rot)/sin(tilt);
				if( arg < -1.0 )
					sazm = 1.5*M_PI + xsazm;
				else if( arg > 1.0  )
					sazm = 0.5*M_PI + xsazm;
				else if( rot < -0.5*M_PI )
					sazm = xsazm - M_PI - asin(arg);
				else if( rot > 0.5*M_PI )
					sazm = xsazm + M_PI - asin(arg);
				else
					sazm = asin(arg) + xsazm;
				if( sazm > 2.0*M_PI )       /* Keep between 0 and 2pi */
					sazm = sazm - 2.0*M_PI;
				else if( sazm < 0.0 )
					sazm = sazm + 2.0*M_PI;
				}
		/* printf("zen=%6.1f azm-sazm=%6.1f tilt=%6.1f arg=%7.4f\n",zen/DTOR,(azm-sazm)/DTOR,tilt/DTOR,arg); */
									/* Find incident angle */
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
			if( arg < -1.0 )
				inc = M_PI;
			else if( arg > 1.0  )
				inc = 0.0;
			else
				inc = acos(arg);
			break;
		case 2:                 /* Two-Axis Tracking */
			tilt = zen;
			sazm = azm;
			inc = 0.0;
			rot = 0.0;
			break;
		}
	angle[0] = inc;           /* Variables returned in array angle[] */
	angle[1] = tilt;
	angle[2] = sazm;
	angle[3] = rot;
	angle[4] = btdiff;
}

#define SMALL 1e-6

void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be null */ )
{
/* added aug2011 by aron dobos. Defines Hay, Davies, Klutcher, Reindl model for diffuse irradiance on a tilted surface
	
	List of Parameters Passed to Function:
	hextra = extraterrestrial irradiance on horizontal surface (W/m2)
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiances (W/m2)
				poa[0]: incident beam
				poa[1]: incident sky diffuse
				poa[2]: incident ground diffuse 
								
	diffc   = diffuse components, if an array is provided
				diffc[0] = isotropic
				diffc[1] = circumsolar
				diffc[2] = horizon brightening*/

	double hb = dn*cos(zen); /* beam irradiance on horizontal */
	double ht = hb+df; /* total irradiance on horizontal */
	if (ht < SMALL) ht = SMALL;
	if (hextra < SMALL) hextra = SMALL;

	double Rb = cos(inc)/cos(zen); /* ratio of beam on surface to beam on horizontal (D&B eq 1.8.1) */
	double Ai = hb/hextra; /* anisotropy index, term for forward scattering circumsolar diffuse (D&B eq 2.16.3) */
	double f = sqrt(hb/ht); /* modulating factor for horizontal brightening correction */
	double s3 = pow( sin( tilt*0.5 ), 3 ); /* horizontal brightening correction */

	/* see ESTIMATING DIFFUSE RADIATION ON HORIZONTAL SURFACES AND TOTAL RADIATION ON TILTED SURFACES
		 Master's Thesis, Douglas T Reindl, 1988, U.Wisc-Madison, Solar Energy Laboratory, http://sel.me.wisc.edu/publications/theses/reindl88.zip */

	double cir = df*Ai*Rb;
	double iso = df*(1-Ai)*0.5*(1+cos(tilt));
	double isohor = df*(1.0-Ai)*0.5*(1.0+cos(tilt))*(1.0+f*s3);

	poa[0] = dn*cos(inc);
	poa[1] = isohor+cir;
	poa[2] = (hb+df)*alb*(1.0-cos(tilt))/2.0;

	//prevent from returning negative poa values, added by jmf 7/28/14
	if (poa[0] < 0) poa[0] = 0;
	if (poa[1] < 0) poa[1] = 0;
	if (poa[2] < 0) poa[2] = 0;
	
	if (diffc != 0)
	{
		diffc[0] = iso;
		diffc[1] = cir;
		diffc[2] = isohor-iso;
	}
}


void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
{
/* added aug2011 by aron dobos. Defines isotropic sky model for diffuse irradiance on a tilted surface
	
	List of Parameters Passed to Function:
	hextra = extraterrestrial irradiance on horizontal surface (W/m2) (unused for isotropic sky)
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiances (W/m2)
				poa[0]: incident beam
				poa[1]: incident sky diffuse
				poa[2]: incident ground diffuse 
				
	diffc   = diffuse components, if an array is provided
				diffc[0] = isotropic
				diffc[1] = circumsolar
				diffc[2] = horizon brightening
				*/

	poa[0] = dn*cos(inc);
	poa[1] = df*(1.0+cos(tilt))/2.0;
	poa[2] = (dn*cos(zen)+df)*alb*(1.0-cos(tilt))/2.0;

	//prevent from returning negative poa values, added by jmf 7/28/14
	if (poa[0] < 0) poa[0] = 0;
	if (poa[1] < 0) poa[1] = 0;
	if (poa[2] < 0) poa[2] = 0;

	if (diffc != 0)
	{
		diffc[0] = poa[1];
		diffc[1] = 0; // no circumsolar
		diffc[2] = 0; // no horizon brightening
	}
}

void perez( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
{
/* Modified aug2011 by aron dobos to split out beam, diffuse, ground for output.
	Total POA is poa[0]+poa[1]+poa[2]

   Defines the Perez function for calculating values of diffuse + direct
	solar radiation + ground reflected radiation for a tilted surface
	and returns the total plane-of-array irradiance(poa).  Function does
	not check all input for valid entries; consequently, this should be
	done before calling the function.  (Reference: Perez et al, Solar
	Energy Vol. 44, No.5, pp.271-289,1990.) Based on original FORTRAN
	program by Howard Bisner.

	Modified 6/10/98 so that for zenith angles between 87.5 and 90.0 degrees,
	the diffuse radiation is treated as isotropic instead of 0.0.

	List of Parameters Passed to Function:
	hextra = extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiances (W/m2)
				poa[0]: incident beam
				poa[1]: incident sky diffuse
				poa[2]: incident ground diffuse 
				
	diffc   = diffuse components, if an array is provided
				diffc[0] = isotropic
				diffc[1] = circumsolar
				diffc[2] = horizon brightening

				*/

													/* Local variables */
	double F11R[8] = { -0.0083117, 0.1299457, 0.3296958, 0.5682053,
							 0.8730280, 1.1326077, 1.0601591, 0.6777470 };
	double F12R[8] = {  0.5877285, 0.6825954, 0.4868735, 0.1874525,
							-0.3920403, -1.2367284, -1.5999137, -0.3272588 };
	double F13R[8] = { -0.0620636, -0.1513752, -0.2210958, -0.2951290,
							-0.3616149, -0.4118494, -0.3589221, -0.2504286 };
	double F21R[8] = { -0.0596012, -0.0189325, 0.0554140, 0.1088631,
							 0.2255647, 0.2877813, 0.2642124, 0.1561313 };
	double F22R[8] = {  0.0721249, 0.0659650, -0.0639588, -0.1519229,
							-0.4620442, -0.8230357, -1.1272340, -1.3765031 };
	double F23R[8] = { -0.0220216, -0.0288748, -0.0260542, -0.0139754,
							 0.0012448, 0.0558651, 0.1310694, 0.2506212 };
	double EPSBINS[7] = { 1.065, 1.23, 1.5, 1.95, 2.8, 4.5, 6.2 };
	double B2=0.000005534,
		EPS,T,D,DELTA,A,B,C,ZH,F1,F2,COSINC,x;
	double CZ,ZC,ZENITH,AIRMASS;

	int i;

	if ( diffc != 0 )
		diffc[0] = diffc[1] = diffc[2] = 0.0;

	if ( dn < 0.0 )           /* Negative values may be measured if cloudy */
		dn = 0.0;

	if ( zen < 0.0 || zen > 1.5271631 ) /* Zen not between 0 and 87.5 deg */
		{
		if( df < 0.0 )
			df = 0.0;
		if ( cos(inc) > 0.0 && zen < 1.5707963 )  /* Zen between 87.5 and 90 */
			{                                      /* and incident < 90 deg   */
			poa[0] = dn * cos(inc);
			poa[1] = df*( 1.0 + cos(tilt) )/2.0;
			poa[2] = 0.0;

			if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
			return;
			}
		else
			{
			poa[0] = 0;
			poa[1] = df*( 1.0 + cos(tilt) )/2.0;   /* Isotropic diffuse only */
			poa[2] = 0.0;
			
			if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
			return;
			}
		}
	else                      /* Zen between 0 and 87.5 deg */
		{
		CZ = cos(zen);
		ZH = ( CZ > 0.0871557 ) ? CZ:0.0871557;    /* Maximum of 85 deg */
		D = df;                /* Horizontal diffuse radiation */
		if ( D <= 0.0 )        /* Diffuse is zero or less      */
			{
			if ( cos(inc) > 0.0 )    /* Incident < 90 deg */
				{
				poa[0] = dn*cos(inc);
				poa[1] = 0.0;
				poa[2] = 0.0;
				return;
				}
			else
				{
				poa[0] = 0;
				poa[1] = 0;
				poa[2] = 0;
				return;
				}
			}
		else                   /* Diffuse is greater than zero */
			{
			ZENITH = zen/DTOR;
			AIRMASS = 1.0 / (CZ + 0.15 * pow(93.9 - ZENITH, -1.253) );
			DELTA = D * AIRMASS / 1367.0;
			T = pow(ZENITH,3.0);
			EPS = (dn + D) / D;
			EPS = (EPS + T*B2) / (1.0 + T*B2);
			i=0;
			while ( i < 7 && EPS > EPSBINS[i] )
				i++;
			x = F11R[i] + F12R[i]*DELTA + F13R[i]*zen;
			F1 = ( 0.0 > x ) ? 0.0:x;
			F2 = F21R[i] + F22R[i]*DELTA + F23R[i]*zen;
			COSINC = cos(inc);
			if( COSINC < 0.0 )
				ZC = 0.0;
			else
				ZC = COSINC;

			// apd 7oct2011: reorganized from original pvwatts code
			// see duffie&beckman 2006, eqn 2.16.14
			A = D*(1-F1)*( 1.0 + cos(tilt) )/2.0; // isotropic diffuse
			B = D*F1*ZC/ZH; // circumsolar diffuse
			C = D*F2*sin(tilt); // horizon brightness term
						
			if (diffc != 0)
			{
				diffc[0] = A;
				diffc[1] = B;
				diffc[2] = C;
			}
			
			// original PVWatts: poa = A + F1*B + F2*C + alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0 + dn*ZC;
			poa[0] = dn*ZC; // beam
			poa[1] = A + B + C; // total sky diffuse
			poa[2] = alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0; // ground diffuse
			return;
			}
		}
}





irrad::irrad()
{
	year=month=day=hour = -999;
	minute=delt=lat=lon=tz=-999;
	radmode=skymodel=track = -1;
	gh=dn=df=alb=tilt=sazm=rlim=-999;
	
	for (int i=0;i<9;i++) sun[i] = std::numeric_limits<double>::quiet_NaN();
	angle[0]=angle[1]=angle[2]=angle[3]=angle[4]= std::numeric_limits<double>::quiet_NaN();
	poa[0]=poa[1]=poa[2]=diffc[0]=diffc[1]=diffc[2] = std::numeric_limits<double>::quiet_NaN();
	tms[0]=tms[1]=tms[2] = -999;
	gcr=std::numeric_limits<double>::quiet_NaN();
	en_backtrack = false;
	ghi = std::numeric_limits<double>::quiet_NaN();
}

int irrad::check()
{
	if (year < 0 || month < 0 || day < 0 || hour < 0 || minute < 0 || delt < 0 || delt > 1) return -1;
	if ( lat < -90 || lat > 90 || lon < -180 || lon > 180 || tz < -15 || tz > 15 ) return -2;
	if ( radmode < 0 || radmode > 1 || skymodel < 0 || skymodel > 2 ) return -3;
	if ( track < 0 || track > 3 ) return -4;
	if ( radmode == 0 && (dn < 0 || dn > 1500 || df < 0 || df > 1500)) return -5;
	if ( radmode == 1 && (gh < 0 || gh > 1500 || dn < 0 || dn > 1500)) return -6;
	if ( alb < 0 || alb > 1 ) return -7;
	if ( tilt < 0 || tilt > 90 ) return -8;
	if ( sazm < 0 || sazm >= 360 ) return -9;
	if ( rlim < -90 || rlim > 90 ) return -10;
	return 0;
}

double irrad::get_ghi()
{
	return ghi;
}

void irrad::get_sun( double *solazi,
	double *solzen,
	double *solelv,
	double *soldec,
	double *sunrise,
	double *sunset,
	int *sunup,
	double *eccfac,
	double *tst,
	double *hextra )
{
	if ( solazi != 0 ) *solazi = sun[0] * (180/M_PI);
	if ( solzen != 0 ) *solzen = sun[1] * (180/M_PI);
	if ( solelv != 0 ) *solelv = sun[2] * (180/M_PI);
	if ( soldec != 0 ) *soldec = sun[3] * (180/M_PI);
	if ( sunrise != 0 ) *sunrise = sun[4];
	if ( sunset != 0 ) *sunset = sun[5];
	if ( sunup != 0 ) *sunup = tms[2];
	if ( eccfac != 0 ) *eccfac = sun[6];
	if ( tst != 0 ) *tst = sun[7];
	if ( hextra != 0 ) *hextra = sun[8];
}

void irrad::get_angles( double *aoi,
	double *surftilt,
	double *surfazi,
	double *axisrot,
	double *btdiff )
{
	if ( aoi != 0 ) *aoi = angle[0] * (180/M_PI);
	if ( surftilt != 0 ) *surftilt = angle[1] * (180/M_PI);
	if ( surfazi != 0 ) *surfazi = angle[2] * (180/M_PI);
	if ( axisrot != 0 ) *axisrot = angle[3] * (180/M_PI);
	if ( btdiff != 0 ) *btdiff = angle[4] * (180/M_PI);
}
	
void irrad::get_poa( double *beam, double *skydiff, double *gnddiff,
	double *isotrop, double *circum, double *horizon )
{
	if ( beam != 0 ) *beam = poa[0];
	if ( skydiff != 0 ) *skydiff = poa[1];
	if ( gnddiff != 0 ) *gnddiff = poa[2];
	if ( isotrop != 0 ) *isotrop = diffc[0];
	if ( circum != 0 ) *circum = diffc[1];
	if ( horizon != 0 ) *horizon = diffc[2];
}

void irrad::set_time( int year, int month, int day, int hour, double minute, double delt_hr )
{
	this->year = year;
	this->month = month;
	this->day = day;
	this->hour = hour;
	this->minute = minute;
	this->delt = delt_hr;
}

void irrad::set_location( double lat, double lon, double tz )
{
	this->lat = lat;
	this->lon = lon;
	this->tz = tz;
}

void irrad::set_sky_model( int skymodel, double albedo )
{
	this->skymodel = skymodel;
	this->alb = albedo;
}

void irrad::set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr )
{
	this->track = tracking;
	this->tilt = tilt_deg;
	this->sazm = azimuth_deg;
	this->rlim = rotlim_deg;
	this->en_backtrack = en_backtrack;
	this->gcr = gcr;
}
	
void irrad::set_beam_diffuse( double beam, double diffuse )
{
	this->dn = beam;
	this->df = diffuse;
	this->radmode = 0;
}

void irrad::set_global_beam( double global, double beam )
{
	this->gh = global;
	this->dn = beam;
	this->radmode = 1;
}

int irrad::calc()
{
	int code = check();
	if ( code < 0 )
		return -100+code;
/*
	calculates effective sun position at current timestep, with delt specified in hours

	sun: results from solarpos
	tms: [0]  effective hour of day used for sun position
			[1]  effective minute of hour used for sun position
			[2]  is sun up?  (0=no, 1=midday, 2=sunup, 3=sundown)
	angle: result from incidence
	poa: result from sky model
	diff: broken out diffuse components from sky model

	lat, lon, tilt, sazm, rlim: angles in degrees
*/
	
	double t_cur = hour + minute/60.0;

	// calculate sunrise and sunset hours in local standard time for the current day
	solarpos( year, month, day, 12, 0.0, lat, lon, tz, sun );

	double t_sunrise = sun[4];
	double t_sunset = sun[5];

	if ( t_cur >= t_sunrise - delt/2.0
		&& t_cur < t_sunrise + delt/2.0 )
	{
		// time step encompasses the sunrise
		double t_calc = (t_sunrise + (t_cur+delt/2.0))/2.0; // midpoint of sunrise and end of timestep
		int hr_calc = (int)t_calc;
		double min_calc = (t_calc-hr_calc)*60.0;

		tms[0] = hr_calc;
		tms[1] = (int)min_calc;
				
		solarpos( year, month, day, hr_calc, min_calc, lat, lon, tz, sun );

		tms[2] = 2;				
	}
	else if (t_cur > t_sunset - delt/2.0
		&& t_cur <= t_sunset + delt/2.0 )
	{
		// timestep encompasses the sunset
		double t_calc = ( (t_cur-delt/2.0) + t_sunset )/2.0; // midpoint of beginning of timestep and sunset
		int hr_calc = (int)t_calc;
		double min_calc = (t_calc-hr_calc)*60.0;

		tms[0] = hr_calc;
		tms[1] = (int)min_calc;
				
		solarpos( year, month, day, hr_calc, min_calc, lat, lon, tz, sun );

		tms[2] = 3;
	}
	else if (t_cur >= t_sunrise && t_cur <= t_sunset)
	{
		// timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)			
		tms[0] = hour;
		tms[1] = (int)minute;
		solarpos( year, month, day, hour, minute, lat, lon, tz, sun );
		tms[2] = 1;
	}
	else
	{			
		tms[0] = -1;
		tms[1] = -1;
		tms[2] = 0;
	}

			
	poa[0]=poa[1]=poa[2] = 0;
	diffc[0]=diffc[1]=diffc[2] = 0;
	angle[0]=angle[1]=angle[2]=angle[3]=angle[4] = 0;

	ghi = 0;

	// do irradiance calculations if sun is up
	if (tms[2] > 0)
	{				
		// compute incidence angles onto fixed or tracking surface
		incidence( track, tilt, sazm, rlim, sun[1], sun[0], en_backtrack, gcr, angle );


		double hextra = sun[8];
		double hbeam = dn*cos( sun[1] ); // calculated beam on horizontal surface: sun[1]=zenith
				
		// check beam irradiance against extraterrestrial irradiance
		if ( hbeam > hextra )
		{
			//beam irradiance on horizontal W/m2 exceeded calculated extraterrestrial irradiance
			return -1;
		}

		// compute beam and diffuse inputs based on irradiance inputs mode
		double ibeam = dn;
		double idiff = 0.0;
		if ( radmode == 0 )  // Beam+Diffuse
			idiff = df;
		else if ( radmode == 1 ) // Total+Beam
			idiff = gh - hbeam;
		else
			return -2; // diffuse or global must be given in additional to beam irradiance

		// compute incident irradiance on tilted surface
		switch( skymodel )
		{
		case 0:
			isotropic( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
			break;
		case 1:
			hdkr( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
			break;
		default:
			perez( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
			break;
		}

		ghi = idiff;
	}

	return 0;

}




static double cosd( double x ) { return cos( DTOR*x ); }
static double sind( double x ) { return sin( DTOR*x ); }
static double tand( double x ) { return tan( DTOR*x ); }
static double acosd( double x ) { return acos(x)/DTOR; }

static double vec_dot(double a[3], double b[3])
{
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

static void vec_cross(double a[3], double b[3], double result[3])
{
	result[0] = a[1] * b[2] - a[2] * b[1];
	result[1] = a[2] * b[0] - a[0] * b[2];
	result[2] = a[0] * b[1] - a[1] * b[0];
}

//a - b
static void vec_diff( double a[3], double b[3], double result[3] )
{
	result[0] = a[0] - b[0];
	result[1] = a[1] - b[1];
	result[2] = a[2] - b[2];
}

static void get_vertices( double axis_tilt, double axis_azimuth, double gcr,				 
				 double vertices[3][4][3], double rotation)
{
	//Get panel vertices for flat panels, no tilt or azimuth, 
	//ordered ccw starting from x+
	//vertices[0] is panel 0
	//vertices[0][1] is corner 1 on panel 0
	//vertices[0][1][2] is coordinate 2(z) for corner 1 on panel 0.
	//All are 0-indexed.  0=x, 1=y, 2=z.

	double width = 1.0;
	double row_spacing = 1.0/gcr - 1.0;
	double length = 10.0;

	for (int i=0; i<3; i++)
	{
		vertices[i][0][0] = width/2 + i*(row_spacing + width);
		vertices[i][0][1] = 0;
		vertices[i][0][2] = 0;
		
		vertices[i][1][0] = width/2 + i*(row_spacing + width);
		vertices[i][1][1] = length;
		vertices[i][1][2] = 0;
			
		vertices[i][2][0] = -width/2 + i*(row_spacing + width);
		vertices[i][2][1] = length;
		vertices[i][2][2] = 0;
		
		vertices[i][3][0] = -width/2 + i*(row_spacing + width);
		vertices[i][3][1] = 0;
		vertices[i][3][2] = 0;
	}
	//We now have vertices for flat panels spaced evenly along the x+ axis.
	
	//Rotate each panel by rotation angle 
	for (int i=0; i<3; i++)
	{
		//Move so that we rotate about y-axis.
		//Perform rotation, then move back.
		double offset = i*(row_spacing + width);
		
		vertices[i][0][0] = vertices[i][0][0] - offset;
		vertices[i][1][0] = vertices[i][1][0] - offset;
		vertices[i][2][0] = vertices[i][2][0] - offset;
		vertices[i][3][0] = vertices[i][3][0] - offset;
		
		//Rotation matrix T is 
		//  cos 0 sin
		//   0  1  0
		// -sin 0 cos
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for(int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
			double oldVertZ = vertices[i][j][2];
			vertices[i][j][0] = oldVertX * cosd(rotation) + oldVertZ * sind(rotation);
			vertices[i][j][2] = oldVertX * -sind(rotation) + oldVertZ * cosd(rotation);
		}
		
		//Translate back to original location after rotation is complete.
		vertices[i][0][0] = vertices[i][0][0] + offset;
		vertices[i][1][0] = vertices[i][1][0] + offset;
		vertices[i][2][0] = vertices[i][2][0] + offset;
		vertices[i][3][0] = vertices[i][3][0] + offset;
	}
	
	
	//Now globally rotate all coords by axis tilt
	for (int i=0; i<3; i++)
	{
		//Move to rotate about x axis.
		//Perform rotation, then move back.
		double offset = length;
		
		vertices[i][0][1] = vertices[i][0][1] - offset;
		vertices[i][1][1] = vertices[i][1][1] - offset;
		vertices[i][2][1] = vertices[i][2][1] - offset;
		vertices[i][3][1] = vertices[i][3][1] - offset;
		
		//Rotation matrix T is 
		// 1   0   0
		// 0  cos sin
		// 0 -sin cos
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for (int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertY = vertices[i][j][1]; //Z coord depends on original y coord.
			double oldVertZ = vertices[i][j][2];
			vertices[i][j][1] = oldVertY * cosd(axis_tilt) + oldVertZ * sind(axis_tilt);
			vertices[i][j][2] = oldVertY * -sind(axis_tilt) + oldVertZ * cosd(axis_tilt);
		}
		
		vertices[i][0][1] = vertices[i][0][1] + offset;
		vertices[i][1][1] = vertices[i][1][1] + offset;
		vertices[i][2][1] = vertices[i][2][1] + offset;
		vertices[i][3][1] = vertices[i][3][1] + offset;
	}
	
	
	//Now globally rotate all coords by axis azimuth
	for (int i=0; i<3; i++)
	{
		//We are rotating about the Z axis, so we don't need to translate.
		
		//Rotation matrix T is 
		//  cos sin 0
		// -sin cos 0
		//   0   0  1
		// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
		for (int j=0; j<4; j++)
		{
			//When we do calculations for new coords, they all depend on old coords.
			double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
			double oldVertY = vertices[i][j][1];
			vertices[i][j][0] = oldVertX * cosd(axis_azimuth) + oldVertY * sind(axis_azimuth);
			vertices[i][j][1] = oldVertX * -sind(axis_azimuth) + oldVertY * cosd(axis_azimuth);
		}
	}
}



static void sun_unit( double sazm, double szen, double sun[3] )
{	
	//Get unit vector in direction of sun
	double solalt = 90 - szen;
		
	if ( sazm >= 0 && sazm <= 90 )
	{
		sun[0] = cosd(solalt)*sind(sazm);
		sun[1] = cosd(solalt)*cosd(sazm);
	}
	else if ( sazm > 90 && sazm <= 180 )
	{
		sun[0] = cosd(solalt)*sind(180-sazm);
		sun[1] = -cosd(solalt)*cosd(180-sazm);
	}
	else if ( sazm > 180 && sazm <= 270 )
	{
		sun[0] = -cosd(solalt)*sind(sazm-180);
		sun[1] = -cosd(solalt)*cosd(sazm-180);
	}
	else
	{
		sun[0] = -cosd(solalt)*sind(360-sazm);
		sun[1] = cosd(solalt)*cosd(360-sazm);
	}
	
	sun[2] = sind(solalt);
		
	//normalize
	double magnitude = sqrt(sun[0]*sun[0] + sun[1]*sun[1] + sun[2]*sun[2]);
	sun[0] = sun[0] / magnitude;
	sun[1] = sun[1] / magnitude;
	sun[2] = sun[2] / magnitude;
}



//Pass a PV system, sun zenith, sun azimuth
//Return true if system is shaded
//False otherwise
double shade_fraction_1x( double solazi, double solzen, 
						 double axis_tilt, double axis_azimuth, 
						 double gcr, double rotation )
{
	//Get unit vector in direction of sun
	
	double sun[3];
	sun_unit( solazi, solzen, sun );

	//For now, assume array has at least 3 rows.
	//This way we can use index 1 and it has a panel on both sides.
	
	//Get our vertices for our array.
	double verts[3][4][3]; //To allocate
	get_vertices( axis_tilt, axis_azimuth, gcr, verts, rotation );
	
	//Find which panel is in the direction of the sun by using dot product.
	//toPrev is a vector from panel 1 to panel 0.
	//toNext is a vector from panel 1 to panel 2.
	//The sun is in the direction of the panel whose vector has a larger positive
	//dot product with the sun direction vector.
	//Store the panel in the direction of the sun from panel 1 in the variable iPanel.
	int iPanel = 0;
	double toPrev[3];
	toPrev[0] = verts[0][0][0] - verts[1][0][0];
	toPrev[1] = verts[0][0][1] - verts[1][0][1];
	toPrev[2] = verts[0][0][2] - verts[1][0][2];
	double toNext[3];
	toNext[0] = verts[2][0][0] - verts[1][0][0];
	toNext[1] = verts[2][0][1] - verts[1][0][1];
	toNext[2] = verts[2][0][2] - verts[1][0][2];
	if (vec_dot(toPrev, sun) < vec_dot(toNext, sun)) iPanel = 2;
	
	
	//Get midpoint of edge of panel 1 on the sun side.
	//This edge is on the same side of panel 1 as iPanel.
	//Store midpoint in midP.
	double midP[3];
	for (int i=0; i<3; i++)
	{
		if (iPanel == 0) midP[i] = (verts[1][2][i] + verts[1][3][i]) / 2;
		else midP[i] = (verts[1][0][i] + verts[1][1][i]) / 2;
	}
	
	//Get normal vector to plane for iPanel.
	//This is easy - just get two vectors in the plane of iPanel and cross them.
	//Use the vectors along the edges out of vertex 0.  That is, edge01 and edge03.
	//Store the result in a vector called normal.
	double normal[3],a1[3],a2[3];
	vec_diff(verts[iPanel][1], verts[iPanel][0], a1);
	vec_diff(verts[iPanel][3], verts[iPanel][0], a2);
	vec_cross(a1, a2, normal);
	
	
	//We want to find the point of intersection of the ray that starts at midP
	//and goes in the direction of the sun.
	//Assume sun is at infinity in direction 'sun'.
	//First make sure that this ray has a unique intersection point.
	double sunDot = vec_dot(normal, sun);
	if (fabs(sunDot) < 0.001) return 0; //sun vector lies in plane
	
	//Now, vector pDir goes from midP to any point in the plane of iPanel.  Vertex 0, say.
	//Project pDir onto the normal to iPanel, and project the sun vector onto the normal vector.
	//The ratio of these projections tells us how fat to move along the sun vector, starting
	//from midP, to get a point in the plane of iPanel.
	double pDir[3];
	vec_diff(verts[iPanel][0], midP, pDir);
	double t = vec_dot(normal, pDir) / vec_dot(normal, sun);
	if (t < 0) return 0; // sun has set (is behind array).
	
	double intersectP[3];
	for (int i=0; i<3; i++)
	{
		intersectP[i] = midP[i] + t * sun[i];
	}
	//intersectP is along the ray from midP to the sun, and lies in the (infinite) plane of iPanel.
	
	//Figure out if intersectP is inside the bounds of the iPanel.
	//This is simple.  If intersectP is on the same side of the edge as the panel for both edges,
	//then it is in the panel.
	//Find a vector from edge to intersectP and take dot product with vector from edge through panel.
	//If dot product is positive, intersectP is on the panel side of that edge.
	//Reuse a1 and a2 from above (as temporary vectors).
	vec_diff(verts[iPanel][3], verts[iPanel][0], a1);
	vec_diff(intersectP, verts[iPanel][0], a2);
	if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.
	
	vec_diff(verts[iPanel][0], verts[iPanel][3], a1);
	vec_diff(intersectP, verts[iPanel][3], a2);
	if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.
	
	// Now we know the panel is shaded, so compute geometric shade fraction	
	double mu[3] = { 0, 0, 0 }; // upper edge midpoint on adjacent panel
	double ml[3] = { 0, 0, 0 }; // lower edge midpoint on adjacent panel
	for (int i=0; i<3; i++)
	{
		if (iPanel == 2) mu[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;
		else             mu[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;
		
		if (iPanel == 2) ml[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;
		else             ml[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;
		
	}
	
	vec_diff(intersectP, mu, a1);
	vec_diff(ml, mu, a2);
	double maga2 = vec_dot(a2,a2);
	double Ab = vec_dot(a1, a2)/maga2;  // geometric shading fraction [0..1]

	return Ab;
}


//Find optimum angle using backtracking.
double backtrack( double solazi, double solzen, 
				 double axis_tilt, double axis_azimuth, 
				 double rotlim, double gcr, double rotation )
{
	//Now do backtracking.
	//This is very straightforward - decrease the rotation as long as we are in shade.
	int iter = 0;
	while( shade_fraction_1x( solazi, solzen, axis_tilt, axis_azimuth, gcr, rotation) > 0 && ++iter < 100)
	{
		//Move closer to flat.
		if (rotation > 0)
		{
			if ( fabs(rotation-1) > fabs(rotlim) )
				break;
			rotation = rotation - 1;
		}	
		else
		{
			if ( fabs(rotation+1) > fabs(rotlim) )
				break;
			rotation = rotation + 1;
		}
	}
	return rotation;
}
