/* PVWATTSV.C 
    10/18/2010 This PVWatts version was received from Ray George,
	 for integration into SSC.  Supports sub-hourly calculation.

	8/22/2007 This is a modification of PVWEB2BI.C so that PVWATTS
	can be run for time intervals less than 60 minutes. List of modifications
	follows:
	1. Added float variable step, equals time step of data, value of 60 or less
	2. Solar position determined at the time stamp minus step/2. If elevation
		> 0.5 deg, then PV calculations performed. Time stamp read in will
		need to include minute when read (variable int min)
	3. Functions celltemp, dcpower, and acpower changed to operate on single
		time stamp data rather than 24 hour data. Variables are now non-array.
	4. Data written to file: Year, Month, Day, Hour, Minute, AC power(W).

	PVWEB2BI.C Version for international data sets   2/9/06
	Reads an array of monthly albedo values from a file instead of computing
	from snowcover.
	Reads tmy data from comma delimited files.
	Reads electric cost data in new format and units.
	Default azimuth set based on if north or south hemisphere
	If latitude below equator, set default tilt to abs value


	PVWEB2b.c sets the tmloss = derate/efffp, and pcrate = dcrate, no longer
	need to normalize output to system size because system is now simulated
	for the system input, not 4 kWac. 4/21/05

	PVWEB2.C Changed from inputting an a.c. rating to inputting a d.c. rating
	and a dc to ac derate factor. a.c. rating = d.c. rating x derate factor.
	Reads inputs from c:\pvweb\pvsystm2.dat. Soiling factor is now in derate
	factor. 4/15/05

	PVWEB.C Version of PV simulation software for testing of code for end
	purpose of being available on the web.   12/7/98

	Added function transpoa to account for reflection losses. 12/8/98
	Added soiling factor of 1% loss and changed array height to 5m. 12/22/98

	Changed temperature degradation from -0.004 to -0.005, increased dc rating
	to accomodate  3/3/99

	Changed rating to 4000 Wac at STC, required dc rating change to 4503.9
	and changed inverter rating to 4500 W.   5/26/99

	Changed soiling loss from 1% to 3%     9/16/99 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "lib_pvwatts.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif


int julian(int yr,int month,int day)    /* Calculates julian day of year */
{
	int i=1,jday=0,k,nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

	if( yr%4 == 0 )                      /* For leap years */
		k = 1;
	else
		k = 0;

	while( i < month )
		{
		jday = jday + nday[i-1];
		i++;
		}
	if( month > 2 )
		jday = jday + k + day;
	else
		jday = jday + day;
	return(jday);
}

double transpoa( double poa, double dn, double inc )
{  /* Calculates the irradiance transmitted thru a PV module cover. Uses King
		polynomial coefficients for glass from 2nd World Conference Paper,
		July 6-10, 1998.                         Bill Marion 12/8/1998 */

	double b0=1.0,
		b1=-2.438e-3,
		b2=3.103e-4,
		b3=-1.246e-5,
		b4=2.112e-7,
		b5=-1.359e-9,
		x = 0.0;

	inc = inc/DTOR;
	if( inc > 50.0 && inc < 90.0 ) /* Adjust for relection between 50 and 90 degrees */
		{
		x = b0 + b1*inc + b2*inc*inc + b3*inc*inc*inc + b4*inc*inc*inc*inc
			 + b5*inc*inc*inc*inc*inc;
		poa = poa - ( 1.0 - x )*dn*cos(inc*DTOR);
		if( poa < 0.0 )
			poa = 0.0;
		}
	return(poa);
}


double celltemp(double inoct,double height,double poa2,double ws2,double ambt2 )
	 /* Defines function to calculate cell temperature, changed 8/22/2007 to
	 work with single time stamp data, also see pvsubs2.c */
{
/*  This function was converted from a PVFORM version 3.3 subroutine
c     this routine estimates the array temperature given the poa radiation,
c     ambient temperature, and wind speed.  it uses an advanced cell temp
c     model developed by m fuentes at snla.  if the poa insolation is eq
c     zero then set cell temp = 999.
c
	passed variables:
		inoct = installed nominal operating cell temperature (deg K)
		height = average array height (meters)
		poa2 = plane of array irradiances (W/m2)
		ws2 = wind speeds (m/s)
		ambt2 = ambient temperatures (deg C)

c  local variables :
c     absorb = absorbtivity
c     backrt = ratio of actual backside heat xfer to theoretical of rack mount
c     boltz = boltzmann's constant
c     cap = capacitance per unit area of module
c     capo = capacitance per unit area of rack mounted module
c     conair = conductivity of air
c     convrt = ratio of total convective heat xfer coef to topside hxc
c     denair = density of air
c     dtime = time step
c     eigen = product of eigen value and time step
c     emmis = emmisivity
		ex = ?
c     grashf = grashoffs number
c     hconv = convective coeff of module (both sides)
c     hforce = forced convective coeff of top side
c     hfree = free convective coeff of top side
c     hgrnd = radiative heat xfer coeff from module to ground
		hsky = ?
c     iflagc = flag to check if routine has been executed
c     reynld = reynolds number
c     suun = insolation at start of time step
c     suno = previous hours insolation
c     tamb = ambient temp
c     tave = average of amb and cell temp
c     tgrat = ratio of grnd temp above amb to cell temp above amb
c     tgrnd = temperature of ground
c     tmod = computed cell temp
c     tmodo = cell temp for previous time step
c     tsky = sky temp
c     visair = viscosity of air
c     windmd = wind speed at module height
c     xlen = hydrodynamic length of module              */

	int j,iflagc=0;
	double pvtemp;  /* The answer returned */
	//double absorb=0.83,backrt,boltz=5.669e-8,cap,capo=11000.0,conair,convrt,denair;
	double absorb=0.83,backrt,boltz=.00000005669,cap=0,capo=11000.0,conair,convrt=0,denair;
	double dtime,eigen,emmis=0.84,grashf,hconv,hforce,hfree,hgrnd,reynld,suun;
	double suno,tamb,tave,tgrat=0,tgrnd=0,tmod,tmodo,tsky,visair,windmd,xlen=0.5;
	double hsky,ex;

/* Set time step to a large number for very first calc. After
	that set time step to 1 (1 hr). Also set prev poa and prev
	module temp for first time through                  */

	dtime=12.0;
	suno=0.0;
	tmodo=293.15;

/* Compute convective coeff, grnd temp ratio, and mod capac one time */

	if( iflagc != 1 )
		{
				/* convective coefficient at noct */
		windmd=1.0;
		tave=(inoct+293.15)/2.0;
		denair=0.003484*101325.0/tave;
		visair=0.24237e-6*pow(tave,0.76)/denair;
		conair=2.1695e-4*pow(tave,0.84);
		reynld=windmd*xlen/visair;
		hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
		grashf=9.8/tave*(inoct-293.15)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
		hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
		hconv=pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);

				/* Determine the ground temperature ratio and the ratio of
					the total convection to the top side convection */
		hgrnd=emmis*boltz*(pow(inoct,2.0)+pow(293.15,2.0))*(inoct+293.15);
		backrt=( absorb*800.0-emmis*boltz*(pow(inoct,4.0)-pow(282.21,4.0))
					-hconv*(inoct-293.15) )/((hgrnd+hconv)*(inoct-293.15));
		tgrnd=pow(pow(inoct,4.0)-backrt*(pow(inoct,4.0)-pow(293.15,4.0)),0.25);
		if( tgrnd > inoct)
			tgrnd=inoct;
		if( tgrnd < 293.15)
			tgrnd=293.15;
		tgrat=(tgrnd-293.15)/(inoct-293.15);
		convrt=(absorb*800.0-emmis*boltz*(2.0*pow(inoct,4.0)-pow(282.21,4.0)
				  -pow(tgrnd,4.0)))/(hconv*(inoct-293.15));

			  /* Adjust the capacitance of the module based on the inoct */
		cap=capo;
		if( inoct > 321.15)
			cap=cap*(1.0+(inoct-321.15)/12.0);
		iflagc=1;
		}

			/* If poa is gt 0 then compute cell temp, else set to 999 */
	if( poa2 > 0.0 )
		{        /* Initialize local variables for insolation and temp */
		tamb=ambt2+273.15;
		suun=poa2*absorb;
		tsky=0.68*(0.0552*pow(tamb,1.5))+0.32*tamb;  /* Estimate sky temperature */

		/*  Estimate wind speed at module height - use technique developed by
			 menicucci and hall (sand84-2530) */
		windmd=ws2*pow(height/9.144,0.2) + 0.0001;
								  /* Find overall convective coefficient */
		tmod=tmodo;
		for(j=0;j<=9;j++)
			{
			tave=(tmod+tamb)/2.0;
			denair=0.003484*101325.0/tave;
			visair=0.24237e-6*pow(tave,0.76)/denair;
			conair=2.1695e-4*pow(tave,0.84);
			reynld=windmd*xlen/visair;
			hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
			if(reynld > 1.2e5)
				hforce=0.0282/pow(reynld,0.2)*denair*windmd*1007.0/pow(0.71,0.4);
			grashf=9.8/tave*fabs(tmod-tamb)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
			hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
			hconv=convrt*pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);
					/* Solve the heat transfer equation */
			hsky=emmis*boltz*(pow(tmod,2.0)+pow(tsky,2.0))*(tmod+tsky);
			tgrnd=tamb+tgrat*(tmod-tamb);
			hgrnd=emmis*boltz*(tmod*tmod+tgrnd*tgrnd)*(tmod+tgrnd);
			eigen=-(hconv+hsky+hgrnd)/cap*dtime*3600.0;
			ex=0.0;
			if(eigen > -10.0)
				ex=exp(eigen);
			tmod=tmodo*ex+((1.0-ex)*(hconv*tamb+hsky*tsky+hgrnd*tgrnd
				  +suno+(suun-suno)/eigen)+suun-suno)/(hconv+hsky+hgrnd);
			}
		tmodo=tmod;  /* Save the new values as initial values for the next hour */
		suno=suun;
		dtime=1.0;

		pvtemp=tmod-273.15;  /* PV module temperature in degrees C */
		}
	else
		pvtemp = 999.0;      /* Default temp for zero irradiance */
	return(pvtemp);
}

										/* Function to determine DC power */
double dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa,double pvt)
{        /* Modified 8/22/07 to pass non-array variables */
/* This function was converted from a PVFORM version 3.3 subroutine but
	uses reference array power ratings instead of reference array
	efficiencies and array sizes to determine dc power.

	Following discussion is original from PVFORM:
	this routine computes the dcpower from the array given a computed
	cell temperature and poa radiation.  it uses a standard power
	degredation technique in which the array efficiency is assumed
	to decrease at a linear rate as a function of temperature rise.
	in most cases the rate of change of efficiency is about .4%perdeg c.

	The code adjusts the array effic if the insolation
	is less than 125w per m2.  the adjustment was suggested by
	fuentes based on observations of plots of effic vs insol at
	several of snla pv field sites.  when insol is less than 125
	the effic is adjusted down at a rate that is porportional
	to that that is observed in the measured field data.  this
	algorithm assumes that the effic is zero at insol of zero.
	this is not true but is a reasonable assumption for a performance
	model.  the net effect of this improvement ranges from less than
	1% in alb to about 2.2% in caribou.  the effect is to reduce
	the overall performace of a fixed tilt system.  tracking
	systems show no measurable diff in performance with respect to
	this power system adjustment.

	passed variables:
		poa = plane of array irradiances (W per m2) for each hour of day
		pvt = temperature of PV cells (deg C)
		reftem =  reference temperature (deg C)
		refpwr =  reference power (W) at reftem and 1000 W per m2 irradiance
		pwrdgr =  power degradation due to temperature, decimal fraction
					(si approx. -0.004, negative means efficiency decreases with
					increasing temperature)
		tmloss =  mismatch and line loss, decimal fraction

	returned variables:
		dc = dc power in watts

	local variables :
		dcpwr1 = dc power(W) from array before mismatch losses      */

	double dcpwr1,dc;

	if( poa > 125.0 )
		dcpwr1=refpwr*(1.0+pwrdgr*(pvt-reftem))*poa/1000.0;
	else if( poa > 0.1 )
		dcpwr1=refpwr*(1.0+pwrdgr*(pvt-reftem))*0.008*poa*poa/1000.0;
	else
		dcpwr1=0.0;

	dc = dcpwr1*(1.0-tmloss);   /* adjust for mismatch and line loss */
	return(dc);

}

double dctoac(double pcrate,double efffp,double dc)
{
/* Revised 8/22/07 to work with single time stamp (non-array) data.
	This function was converted from a PVFORM version 3.3 subroutine
	this routine computes the ac energy from the inverter system.
	it uses a model developed by leeman and menicucci of snla.
	the model is based on efficiency changes of typical pcu systems
	as a function of the load on the system.  these efficiency changes
	were determined through numerous measurements made at snla.
	the model is determined by fitting a curve through a set of pcu
	efficiency measurements ranging from inputs of 10% of full power
	to 100% of full power.  the equation is a 3rd order polynomial.
	between 10% and 0% a linear change is assumed ranging to an efficiency
	of -1.5% at 0% input power.

	passed variables:
		dc = dc power(W)
		pcrate = rated output of inverter in ac watts
		efffp = efficiency of inverter at full power, decimal fraction (such as 0.10)
	local variables :
		dcrtng = equivalent dc rating of pcu
		effrf = efficiency of pcu after adjustment
		percfl = percent of full load the inverter is operating at
		rateff = ratio of eff at full load / ref eff at full load
	returned variable:
		ac = ac power(W)  */
	double dcrtng,effrf,percfl,rateff,ac;

/*   Compute the ratio of the effic at full load given by the user and
	  the reference effic at full load. this will be used later to compute
	  the pcu effic for the exact conditions specified by the user.  */

	rateff=efffp/0.91;

/*   The pc rating is an ac rating so convert it to dc by dividing it
	  by the effic at 100% power. */

	dcrtng=pcrate/efffp;

	if( dc > 0.0 )
		{        /* Determine the reference efficiency based on the
						percentage of full load at input. */
		percfl=dc/dcrtng;
		if ( percfl <= 1.0 )
			{   /* if the percent of full power falls in the range of .1 to 1. then
					 use polynomial to estimate effic, else use linear equation. */
			if( percfl >= 0.1 )
				{
				effrf=0.774+(0.663*percfl)+(-0.952*percfl*percfl)+(0.426*percfl*percfl*percfl);
				if(effrf > 0.925)
					effrf=0.925;
				}
			else       /* percent of full power less than 0.1 */
				{
				effrf=(8.46*percfl)-0.015;
				if(effrf < 0.0)
					effrf=0.0;
				}
			/* compute the actual effic of the pc by adjusting according
				to the user input of effic at max power then compute power. */
			effrf=effrf*rateff;
			ac=dc*effrf;
			}
		else
			ac=pcrate;  /* On an overload condition set power to rated pc power */
		}
	else         /* dc in equals 0 */
		ac = 0.0;

	return(ac);
}

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[8])
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
	sunn[7] = tst = true solar time (hrs)                */

	int jday,delta,leap;                           /* Local variables */
	double zulu,jd,time,mnlong,mnanom,
			eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
			E,ws,sunrise,sunset,Eo,tst;
	double arg;

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

	sunn[0] = azm;                        /* Variables returned in array sunn[] */
	sunn[1] = 0.5*M_PI - elv;               /*  Zenith */
	sunn[2] = elv;
	sunn[3] = dec;
	sunn[4] = sunrise;
	sunn[5] = sunset;
	sunn[6] = Eo;
	sunn[7] = tst;
}


void incident2(int mode,double tilt,double sazm,double rlim,double zen,double azm,double angle[3])
{
/* This function calculates the incident angle of direct beam radiation to a
	surface for a given sun position, latitude, and surface orientation. The
	modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
	Azimuth angles are for N=0 or 2pi, E=pi/2, S=pi, and W=3pi/2.  8/13/98

	List of Parameters Passed to Function:
	mode   = 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking
	tilt   = tilt angle of surface from horizontal in degrees (mode 0),
				or tilt angle of tracker axis from horizontal in degrees (mode 1),
				MUST BE FROM 0 to 90 degrees.
	sazm   = surface azimuth in degrees of collector (mode 0), or surface
				azimuth of tracker axis (mode 1) with axis azimuth directed from
				raised to lowered end of axis if axis tilted.
	rlim   = plus or minus rotation in degrees permitted by physical constraints
				of tracker, range is 0 to 180 degrees.
	zen    = sun zenith in radians, MUST BE LESS THAN PI/2
	azm    = sun azimuth in radians, measured east from north

	Parameters Returned:
	angle[]  = array of elements to return angles to calling function
	angle[0] = inc  = incident angle in radians
	angle[1] = tilt = tilt angle of surface from horizontal in radians
	angle[2] = sazm = surface azimuth in radians, measured east from north  */

	/* Local variables: rot is the angle that the collector is rotated about the
	axis when viewed from the raised end of the 1-axis tracker. If rotated
	counter clockwise the angle is negative. Range is -180 to +180 degrees.
	When xsazm = azm : rot = 0, tilt = xtilt, and sazm = xsazm = azm  */

	double arg,inc=0,xsazm,xtilt,rot;

	switch ( mode )
		{
		case 0:                 /* Fixed-Tilt */
			tilt = tilt*DTOR;    /* Change tilt and surface azimuth to radians */
			sazm = sazm*DTOR;
			arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
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
			break;
		}
	angle[0] = inc;           /* Variables returned in array angle[] */
	angle[1] = tilt;
	angle[2] = sazm;
}

double perez( double dn,double df,double alb,double inc,double tilt,double zen )
{
/* Defines the Perez function for calculating values of diffuse + direct
	solar radiation + ground reflected radiation for a tilted surface
	and returns the total plane-of-array irradiance(poa).  Function does
	not check all input for valid entries; consequently, this should be
	done before calling the function.  (Reference: Perez et al, Solar
	Energy Vol. 44, No.5, pp.271-289,1990.) Based on original FORTRAN
	program by Howard Bisner.

	Modified 6/10/98 so that for zenith angles between 87.5 and 90.0 degrees,
	the diffuse radiation is treated as isotropic instead of 0.0.

	List of Parameters Passed to Function:
	dn     = direct normal radiation (W/m2)
	df     = diffuse horizontal radiation (W/m2)
	alb    = surface albedo (decimal fraction)
	inc    = incident angle of direct beam radiation to surface in radians
	tilt   = surface tilt angle from horizontal in radians
	zen    = sun zenith angle in radians

	Variable Returned
	poa    = plane-of-array irradiance (W/m2), sum of direct beam and sky
				and ground-reflected diffuse */

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
		EPS,T,D,DELTA,A,B,C,ZH,F1,F2,COSINC,poa,x;
	double CZ,ZC,ZENITH,AIRMASS;

	int i;

	if ( dn < 0.0 )           /* Negative values may be measured if cloudy */
		dn = 0.0;
	if ( zen < 0.0 || zen > 1.5271631 ) /* Zen not between 0 and 87.5 deg */
		{
		if( df < 0.0 )
			df = 0.0;
		if ( cos(inc) > 0.0 && zen < 1.5707963 )  /* Zen between 87.5 and 90 */
			{                                      /* and incident < 90 deg   */
			poa = df*( 1.0 + cos(tilt) )/2.0 + dn*cos(inc);
			return(poa);
			}
		else
			{
			poa = df*( 1.0 + cos(tilt) )/2.0;   /* Isotropic diffuse only */
			return(poa);
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
				poa = 0.0 + dn*cos(inc);
				return(poa);
				}
			else
				{
				poa = 0.0;
				return(poa);
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
			A = D*( 1.0 + cos(tilt) )/2.0;
			B = ZC/ZH*D - A;
			C = D*sin(tilt);
			poa = A + F1*B + F2*C + alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0 + dn*ZC;
			return(poa);
			}
		}
}







#if __PVWATTS_MAIN_CODE__ // should never be defined, code included below for reference


main(int argc, char **argv)
//main()
{
	FILE *fp_in,*fp_in2,*fp_out;
	char wban[7],city[23],state[4];   /* 1/25/06 */
	char swban[7],cwban[7]="xxxxxx",dwban[7]="xxxxxx"; /* Changed from 5 to 6 characters, added dwban 1/24/06 */
	int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
	int i,j,k,m,n,elv,outmode,csp,year; /*Removed dlat,mlat,dlng,mlng 1/26/06 */
	int yr,mn,dy,hr,ethor,etdn,mode;
	char name[27]="c:\\pvweb\\intmys\\",stmy[5]=".tmy",dum1[50],dum2[50]; /* 1/25/06 */
    char io[200];                                       // added 2006.03.08 MLA; needed for reading input files on UNIX system

	float lat,lng,tz,minute,sunn[8],angle[3];
	float dn,df,alb,DTOR=0.01745329,tilt,sazm;
	float poa,ambt,wind,pvt,dc,ac,tpoa;
	double tmp,tmp2,inoct,height;
	float reftem,refpwr,pwrdgr,tmloss,pcrate,efffp;
	float cost,rlim=45.0,acrate;
	float dcrate,derate;    /* d.c. rating and derate factor 4/15/05 */
	float malb[12],data[11];         /* monthly albedo, input data 1/24/06 */
	int ii;                 /* index counter 1/25/06 */
	float step = 60.0;      /* Time stamp interval of data in minutes 8/22/07*/
	int min = 0;            /* Minute of time stamp, need to read in 8/22/07 */

										/* PV RELATED SPECIFICATIONS */
	inoct = 45.0 + 273.15;     /* Installed normal operating cell temperature (deg K) */
	height = 5.0;              /* Average array height (meters) */
	reftem = 25.0;             /* Reference module temperature (deg C) */
	pwrdgr = -0.005;           /* Power degradation due to temperature (decimal fraction), si approx -0.004 */
	efffp = 0.92;              /* Efficiency of inverter at rated output (decimal fraction) */
	/* Rated ac power for 1000 W/m2 and 25 deg C cell temperature (STC) is 4000 watts */

	//fp_in = fopen("c:\\pvweb\\station.num","r" ); /* Open the file to read station ID# */
	//if( fp_in != NULL )     /* Check for read errors */
		//fscanf( fp_in,"%s",&swban);
	//else
	//	printf("Error in reading file: station.num\n");
	//fclose (fp_in);

	//fp_in = fopen("c:\\pvweb\\pvsystm2.dat","r" ); /* Open the file to read pv system specs 4/15/05 */
	//if( fp_in != NULL )     /* Check for read errors */
		//fscanf( fp_in,"%f %f %f %d %f %f",&dcrate,&derate,&cost,&mode,&tilt,&sazm);
	//else
		//printf("Error in reading file: pvsystm2.dat\n");
	//fclose (fp_in);
	   // short circuit if too few args
   if( argc < 24)
   {
 //     PrintUsageMessage();
      return(-1);
   }
  	     strcpy(wban, argv[1]);
  	     strcpy(name, argv[2]);
  	     strcpy(io, argv[3]);
  	     lng=atof(io);
  	     strcpy(io, argv[4]);
  	     lat=atof(io);
  	     strcpy(io, argv[5]);
  	     elv=atoi(io);
  	     strcpy(io, argv[6]);
  	     tz=atof(io);
  	     strcpy(io, argv[7]);
  	     mode=atoi(io);
  	     strcpy(io, argv[8]);
  	     sazm=atof(io);
  	     strcpy(io, argv[9]);
  	     tilt=atof(io);
  	     strcpy(io, argv[10]);
  	     year=atoi(io);
  	     strcpy(io, argv[11]);
  	     outmode=atoi(io);
  	     strcpy(io, argv[12]);
  	     step=atof(io);
  	for(i=0;i<=12;i++)
	{
  	     strcpy(io, argv[13+i]);
  	     malb[i]=atof(io);
   }
 	  csp=0;
	  if (outmode == -1) {
	  	csp = 1;
	  	outmode = 1;
	  }

    // added following three parameter reads 2006.03.08 MLA	
		
		derate = 0.77;
	
	dcrate = 1.0;
	/* Cost of Electricity */
	cost = 0.08;
		
	
	if (cost == NULL)
	  cost = -99.0;
  sprintf(dum1,"pvw_%s_%d_%2.1f.csv",wban,year,step);
	fp_out = fopen(dum1,"w" );  /* Open file to write */
	/* strncpy( name+12,swban,5 );   1/25/06   /* Copy WBAN no to .tm2 file name */
	//strcat( name,swban );     /* Concatenate wban# to name 1/25/06*/
	//strcat( name,stmy );      /* Concatenate .tmy to name 1/25/06*/
	//printf("%s\n",name);

	fp_in2 = fopen( name,"r");       /* Open .tm2 file */
	if( fp_in2 != NULL )     /* Check for read errors */
		{                     /* Read site header */
		fscanf( fp_in2, "%s",&dum2);     /* 1/25/06 */
		//fscanf( fp_in2, "%s %s %s",&wban,&state,&city);     /* 1/25/06 */
		//fscanf( fp_in2, "%f,%f,%f,%d,",&tz,&lat,&lng,&elv); /* 1/25/06 */

		if( dcrate < 0.49999 || dcrate > 1000.01 )  /* Use defaults if out of range */
			dcrate = 4.0;              /* changed from acrate 4/15/05 */
		if( derate < 0.09999 || derate > 0.96001 )
			derate = 0.77;             /* default ac to dc derate 1/24/06 */

		acrate = dcrate*derate;       /* ac rating 4/15/05 */
		pcrate = dcrate*1000.0;       /* 4/21/05 Rated output of inverter in a.c. watts */
		refpwr = dcrate*1000.0;       /* 4/21/05 ref power = nameplate in watts */
		tmloss = 1.0 - derate/efffp;  /* All losses except inverter,decimal */

			/* Read in monthly albedo data 1/24/06 */
		//fp_in = fopen("c:\\pvweb\\albedos.dat","r" ); /* Open the file to read albedos */
		//while( feof(fp_in) == 0 && strcmp(swban,dwban) != 0 )
			//{
			//fscanf( fp_in,"%s %s %s",&dwban,&dum1,&dum2 );
			//for(i=0;i<=11;i++)
				//fscanf( fp_in,"%f",&malb[i]);
			//}
		//fclose (fp_in);
		//if( strcmp(swban,dwban) != 0 )
			//printf("Error reading in albedo data\n");

		if( mode < 0 || mode > 2 )
			mode = 0;
		if( tilt < -0.0001 || tilt > 90.0001 )
			tilt = fabs(lat);     /* Keep south lat tilt positive 1/26/06 */
		if( sazm < -0.0001 || sazm > 360.0001 )
			{    /* Base azimuth depending on north or south of equator 1/26/06 */
			if( lat >= 0.0 )
				sazm = 180.0;
			else
				sazm = 0.0;
			}

		fprintf(fp_out,"Station Identification\n");
		fprintf(fp_out,"WBAN#:           %s\n",wban);
		//fprintf(fp_out,"City:            %s\n",city);
		//fprintf(fp_out,"State:           %s\n",state);
		fprintf(fp_out,"Latitude(deg):   %6.2f\n",lat);
		fprintf(fp_out,"Longitude(deg):  %6.2f\n",lng);
		fprintf(fp_out,"Elevation(m):    %4d\n",elv);
		fprintf(fp_out,"PV System Specifications\n");
		fprintf(fp_out,"DC Rating(kW):   %7.2f\n",dcrate);
		fprintf(fp_out,"dc to ac derate factor: %6.3f\n",derate);
		fprintf(fp_out,"AC Rating(kW):   %7.2f\n",acrate);
		if( mode == 0 )
			fprintf(fp_out,"Array Type:      Fixed Tilt\n");
		else if( mode == 1 )
			fprintf(fp_out,"Array Type:      1-Axis Tracking\n");
		else if( mode == 2 )
			fprintf(fp_out,"Array Type:      2-Axis Tracking\n");
		if( mode == 0 || mode == 1 )
			{
			fprintf(fp_out,"Array Tilt(deg):    %5.1f\n",tilt);
			fprintf(fp_out,"Array Azimuth(deg): %5.1f\n",sazm);
			}
		else
			{
			fprintf(fp_out,"Array Tilt(deg):    N/A\n");
			fprintf(fp_out,"Array Azimuth(deg): N/A\n");
			}

		fprintf(fp_out,"\nYear,Month,Day,Hour,Minute,AC,DC,RadPOA\n"); /*Header */

		while( feof(fp_in2) == 0 )  /* Read until reach end of file */
			{
							/* 8/22/2007 for sub hourly data, need to read*/
							/* minute variable as int, hard coded here */
			//min = 0;       /* Ray, your code will read minute instead */
			fscanf(fp_in2,"%d,%d,%d,%d,%d,",&yr,&mn,&dy,&hr,&min );   /* 1/25/06 */
			for( ii=0;ii<=4;ii++ )                       /* 1/25/06 */
				fscanf(fp_in2,"%f,",&data[ii]);            /* 1/25/06 */
			if( feof(fp_in2) == 0 ) /* Not at end of file */
			{
				dn=data[1];          /* Direct radiation 1/25/06 */
				df=data[2];          /* Diffuse radiation 1/25/06 */
				ambt=data[3];        /* Ambient dry bulb temperature(C) 1/25/06 */
				wind=data[4];        /* Wind speed(m/s) 1/25/06 */
				poa=-1000.0;
        if (df < -999.0) { /* missing df, use POA from input record */
        	 poa = data[0];
        	 if (poa < 0) {poa = 0.0;}
				} else {
           if (dn < 0.0) {dn = 0.0;}
           if (df < 0.0) {df = 0.0;}
        }
						/* For call, adjust time to midpoint of time step 8/22/07 */
				minute = min - 0.5*step;
				solarpos(yr,mn,dy,hr,minute,lat,lng,tz,sunn);
				if( sunn[2] > 0.0087 )   /* Sun elevation > 0.5 degrees */
					{
					if (poa < 0) {  /* calculate POA from direct and diffuse */
					   incident2(mode,tilt,sazm,rlim,sunn[1],sunn[0],angle); /* Calculate incident angle */
					   poa = perez( dn,df,malb[mn-1],angle[0],angle[1],sunn[1] ); /* Incident solar radiation, using monthly albedo 1/25/06 */
				   }
				   if (dn > 0) {
				   	 /* Have valid poa and dn, calculate tpoa */ 
					   tpoa = transpoa( poa,dn,angle[0]);  /* Radiation transmitted thru module cover */
					 } else {
					 	 /* dn 0 or missing, assume NO glass cover on module */
					 	 tpoa = poa;
					 }
					pvt = celltemp(inoct,height,poa,wind,ambt );
					dc =dcpowr(reftem,refpwr,pwrdgr,tmloss,tpoa,pvt);
					ac = dctoac(pcrate,efffp,dc);
					}
				else     /* Night time */
					{
					poa=0.0;             /* Plane-of-array radiation */
					tpoa=0.0;            /* Transmitted radiation */
					pvt = 999.0;         /* Modeled cell temperature */
					dc=0.0;              /* DC power */
					ac=0.0;              /* AC power */
					}

				fprintf(fp_out,"%4d,%2d,%2d,%2d,%2d,%4.1f,%4.1f,%4.1f\n",
					yr,mn,dy,hr,min,ac,dc,poa );
				}  /* if not at end of file loop */
			}  /* while not at end of file loop */
		}
	else
		printf("Error in reading TMY2file\n");
	fclose (fp_in2);
	fclose (fp_out);
}

#endif
