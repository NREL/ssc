#ifndef __lib_pvwatts_h
#define __lib_pvwatts_h

#define PVWATTS_INOCT (45.0+273.15)
#define PVWATTS_HEIGHT 5.0
#define PVWATTS_REFTEM 25.0
#define PVWATTS_PWRDGR -0.005
#define PVWATTS_EFFFP 0.92
#define PVWATTS_ROTLIM 45.0
#define PVWATTS_ALBEDO 0.2

double transpoa( double poa,double dn,double inc );
double celltemp(double inoct,double height,double poa2,double ws2,double ambt2 );
double dcpowr(double reftem,double refpwr,double pwrdgr,double tmloss,double poa,double pvt);
double dctoac(double pcrate,double efffp,double dc);

#endif
