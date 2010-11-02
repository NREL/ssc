#ifndef __lib_pvwatts_h
#define __lib_pvwatts_h

#define PVWATTS_INOCT (45.0+273.15)
#define PVWATTS_HEIGHT 5.0
#define PVWATTS_REFTEM 25.0
#define PVWATTS_PWRDGR -0.005
#define PVWATTS_EFFFP 0.92
#define PVWATTS_ROTLIM 45.0
#define PVWATTS_ALBEDO 0.2

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[8]);
void incident2(int mode,double tilt,double sazm,double rlim,double zen,double azm,double angle[3]);
double perez( double dn,double df,double alb,double inc,double tilt,double zen );
double transpoa( double poa,double dn,double inc );
double celltemp(double inoct,double height,double poa2,double ws2,double ambt2 );
double dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa,double pvt);
double dctoac(double pcrate,double efffp,double dc);

#endif
