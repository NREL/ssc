#include <stdio.h>
#include <math.h>

#include "lib_cec6par.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279
#endif

/******** BEGIN GOLDEN METHOD CODE FROM NR3 *********/

#define GOLD 1.618034
#define GLIMIT 100.0
#define TINY 1.0e-20
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define FMAX(a,b) ((a)>(b)?(a):(b))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

static void mnbrak(double *ax, double *bx, double *cx, double *fa, double *fb, double *fc,
	double (*func)(double, void *), void *data)
{
	double ulim,u,r,q,fu,dum;

	*fa=(*func)(*ax, data);
	*fb=(*func)(*bx, data);
	if (*fb > *fa) {
		SHFT(dum,*ax,*bx,dum)
		SHFT(dum,*fb,*fa,dum)
	}
	*cx=(*bx)+GOLD*(*bx-*ax);
	*fc=(*func)(*cx,data);
	while (*fb > *fc) {
		r=(*bx-*ax)*(*fb-*fc);
		q=(*bx-*cx)*(*fb-*fa);
		u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
			(2.0*SIGN(FMAX(fabs(q-r),TINY),q-r));
		ulim=(*bx)+GLIMIT*(*cx-*bx);
		if ((*bx-u)*(u-*cx) > 0.0) {
			fu=(*func)(u,data);
			if (fu < *fc) {
				*ax=(*bx);
				*bx=u;
				*fa=(*fb);
				*fb=fu;
				return;
			} else if (fu > *fb) {
				*cx=u;
				*fc=fu;
				return;
			}
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u,data);
		} else if ((*cx-u)*(u-ulim) > 0.0) {
			fu=(*func)(u,data);
			if (fu < *fc) {
				SHFT(*bx,*cx,u,*cx+GOLD*(*cx-*bx))
				SHFT(*fb,*fc,fu,(*func)(u,data))
			}
		} else if ((u-ulim)*(ulim-*cx) >= 0.0) {
			u=ulim;
			fu=(*func)(u,data);
		} else {
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u,data);
		}
		SHFT(*ax,*bx,*cx,u)
		SHFT(*fa,*fb,*fc,fu)
	}
}
#undef GOLD
#undef GLIMIT
#undef TINY
#undef SHFT
#undef NRANSI

#define R 0.61803399
#define C (1.0-R)
#define SHFT2(a,b,c) (a)=(b);(b)=(c);
#define SHFT3(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);

static double golden(double ax, double bx, double (*f)(double,void*), void *data, double tol, double *xmin )
{
	double f1,f2,x0,x1,x2,x3,cx, fa, fb, fc;

	mnbrak(&ax, &bx, &cx, &fa, &fb, &fc, f, data );

	x0=ax;
	x3=cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1=bx;
		x2=bx+C*(cx-bx);
	} else {
		x2=bx;
		x1=bx-C*(bx-ax);
	}
	f1=(*f)(x1,data);
	f2=(*f)(x2,data);
	while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2))) {
		if (f2 < f1) {
			SHFT3(x0,x1,x2,R*x1+C*x3)
			SHFT2(f1,f2,(*f)(x2,data))
		} else {
			SHFT3(x3,x2,x1,R*x2+C*x0)
			SHFT2(f2,f1,(*f)(x1,data))
		}
	}
	if (f1 < f2) {
		*xmin=x1;
		return f1;
	} else {
		*xmin=x2;
		return f2;
	}
}
#undef C
#undef R
#undef SHFT2
#undef SHFT3

/******** END GOLDEN METHOD CODE FROM NR2 *********/

#define min(a,b) ((a)<(b)?(a):(b))
#define max(a,b) ((a)>(b)?(a):(b))
#define sinD(x) sin((x)*0.017453292519943295769236907684886)
#define cosD(x) cos((x)*0.017453292519943295769236907684886)
#define tanD(x) tan((x)*0.017453292519943295769236907684886)
#define asinD(x) (57.295779513082320876798154814105*asin(x))
#define acosD(x) (57.295779513082320876798154814105*acos(x))
#define atanD(x) (57.295779513082320876798154814105*atan(x))


static double openvoltage_194( double Voc0, double a, double IL, double IO, double RS, double RSH )
{
/*
	C     Iterative solution for open-circuit voltage.  Explicit algebraic solution
	C     not possible in 5-parameter model
*/	
	double VocLow = 0;
	double VocHigh = Voc0 * 1.5;
	
	double Voc = Voc0; // initial guess
	
	int niter = 0;
	while( fabs(VocHigh-VocLow) > 0.001 )
	{
		double I = IL - IO*(exp(Voc/a)-1) - Voc/RSH;
		if (I < 0) VocHigh = Voc;
		if (I > 0) VocLow = Voc;
		Voc = (VocHigh+VocLow)/2;

		if (++niter > 5000)
			return -1.0;
	}
	return Voc;	
}

static double current_194( double V, double IMR, double A, double IL, double IO, double RS, double RSH )
{
/*
	C     Iterative solution for current as a function of voltage using
	C     equations from the five-parameter model.  Newton's method is used
	C     to converge on a value.  Max power at reference conditions is initial
	C     guess. 
*/
	double IOLD = 0.0;
	double V_MODULE = V;	
	
	//C**** first guess is max.power point current
	double INEW = IMR;
	while( fabs(INEW-IOLD) > 0.0001)
	{
		IOLD = INEW;
		double F = IL-IOLD-IO*(exp((V_MODULE+IOLD*RS)/A)-1.0) - (V_MODULE+IOLD*RS)/RSH;
		double FPRIME = -1.0-IO*(RS/A)*exp((V_MODULE+IOLD*RS)/A)-(RS/RSH);
		INEW = max(0.0,(IOLD-(F/FPRIME)));
	}
	
	return INEW;
}

static const double KB = 8.618e-5; // Boltzmann constant [eV/K] note units
static const double a0 =0.918093, a1=0.086257, a2=-0.024459, a3=0.002816, a4=-0.000126; // !Air mass modifier coefficients as indicated in DeSoto paper
static const double T_prop=315.0, k_air=0.02676, mu_air=1.927E-5, Pr_air=0.724;  // !Viscosity in units of N-s/m^2
static const double EmisC = 0.84, EmisB = 0.7;  // Emissivities of glass cover, backside material
static const double sigma = 5.66961E-8, cp_air = 1005.5;

static const double n_cover = 1.526;   // !refractive index of glass
static const double l_thick = 0.002;   // !thickness of glass cover
static const double k_trans = 4; // proportionality constant for 


static const double Tc_ref = (25+273.15); // 25 'C
static const double I_ref = 1000; // 1000 W/m2
static const double Tamb_noct = 20;  // 20 Ambient NOCT temp ('C)
static const double I_noct = 800; // 800 NOCT Irradiance W/m2
static const double TauAlpha = 0.9; // 0.9
static const double eg0 = 1.12; // 1.12

static double transmittance( double incangdeg, /* incidence angle of incoming radiation (deg) */
		double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
		double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law */
		double l_thick ) /* thickness of cover material (m), usually 2 mm for typical module */
{
	// reference: duffie & beckman, Ch 5.3
	
	double theta1 = incangdeg * M_PI/180.0;
	double theta2 = asin( 1.0 / n_cover * sin(theta1 ) ); // snell's law, assuming n_air = 1.0
	// fresnel's equation for non-reflected unpolarized radiation as an average of perpendicular and parallel components
	double tr = 1 - 0.5 *
			( pow( sin(theta2-theta1), 2 )/pow( sin(theta2+theta1), 2)
			+ pow( tan(theta2-theta1), 2 )/pow( tan(theta2+theta1), 2 ) );
	
	return tr * exp( -k * l_thick / cos(theta2) );
}

static double irradiance_through_cover(
	double theta,
	double theta_z,
	double tilt,
	double G_beam,
	double G_sky,
	double G_gnd
	)
{
	// establish limits on incidence angle and zenith angle
	if (theta < 1) theta = 1;
	if (theta > 89) theta = 89;
		
	if (theta_z > 86.0) theta_z = 86.0; // !Zenith angle must be < 90 (?? why 86?)
	if (theta_z < 0) theta_z = 0; // Zenith angle must be >= 0
		
	// incidence angle modifier calculations to determine
	// effective irradiance transmitted through glass cover

	// transmittance at angle normal to surface (0 deg), use 1 (deg) to avoid numerical probs.
	double ta_norm = transmittance( 1.0, n_cover, k_trans, l_thick );
	
	// transmittance of beam radiation, at incidence angle
	double ta_beam = transmittance( theta, n_cover, k_trans, l_thick );

	// transmittance of sky diffuse, at modified angle by (D&B Eqn 5.4.2)
	double theta_sky = 59.7 - 0.1388*tilt + 0.001497*tilt*tilt;
	double ta_sky = transmittance( theta_sky, n_cover, k_trans, l_thick );
	
	// transmittance of ground diffuse, at modified angle by (D&B Eqn 5.4.1)
	double theta_gnd = 90.0 - 0.5788*tilt  + 0.002693*tilt*tilt;
	double ta_gnd = transmittance( theta_gnd, n_cover, k_trans, l_thick );

	// calculate component incidence angle modifiers, D&B Chap. 5 eqn 5.12.1, DeSoto'04
	double Kta_beam = ta_beam / ta_norm;
	double Kta_sky = ta_sky / ta_norm;
	double Kta_gnd = ta_gnd / ta_norm;
	
	// total effective irradiance absorbed by solar cell
	double Geff_total = G_beam*Kta_beam + G_sky*Kta_sky + G_gnd*Kta_gnd;
	
	if (Geff_total < 0) Geff_total = 0;
	
	return Geff_total;
}

int cec6par_cell_temp_function (
	// INPUTS
	void *module_spec, // pointer to cec6par_module_t
	pv_input_t *input,
	
	module_power_function f_modpwr,
	void *module_spec_modpwr,
	
	// OUTPUTS
	double *celltemp )
{
	double T_cell = input->tamb + 273.15;
	
	double Geff_total = irradiance_through_cover(
		input->incang,
		input->zenith,
		input->tilt,
		input->ibeam,
		input->idiff,
		input->ignd );		
	
	if (Geff_total > 1.0 )
	{
		cec6par_module_t *modspec = (cec6par_module_t*)module_spec;		
		double G_total = input->ibeam + input->idiff + input->ignd;		
		double eff_ref = modspec->Imp * modspec->Vmp / ( I_ref*modspec->Area );
		double tau_al = fabs(TauAlpha);
		double W_spd = input->wspd;		
		if (W_spd < 0.001) W_spd = 0.001;		
		if (G_total > 0) tau_al *= Geff_total/G_total;		
		T_cell = (input->tamb+273.15) + (G_total/I_noct * (modspec->T_noct-Tamb_noct) * (1.0-eff_ref/tau_al))*9.5/(5.7 + 3.8*W_spd);
	}
	
	*celltemp = T_cell - 273.15;

	return 0;
}

struct refparm
{
	double a,Il,Io,Rs,Rsh;
};

static double powerfunc( double V, void *_d )
{
	struct refparm *r = (struct refparm*)_d;
	return -V*current_194( V, 0.9*r->Il, r->a, r->Il, r->Io, r->Rs, r->Rsh );
}

 int cec6par_module_power_function (
	// INPUTS
	void *module_spec, // pointer to cec6par_module_t
	int mode, 
	double opvoltage, 
	double celltemp,	
	pv_input_t *input,
	
	// OUTPUTS
	double *power, double *voltage, double *current, 
	double *eff, double *voc, double *isc )
{
	cec6par_module_t *modspec = (cec6par_module_t*) module_spec;
	
	double muIsc = modspec->alpha_isc * (1-modspec->Adjust/100);
	double muVoc = modspec->beta_voc * (1+modspec->Adjust/100);
	
	double T_amb = input->tamb + 273.15;  // convert to Kelvin from C	
	
	/* initialize output first */
	*power = *voltage = *current = *eff = *voc = *isc = 0.0;
	
	double G_total = input->ibeam + input->idiff + input->ignd; // total incident irradiance on tilted surface, W/m2
	
	
	double Geff_total = irradiance_through_cover(
		input->incang,
		input->zenith,
		input->tilt,
		input->ibeam,
		input->idiff,
		input->ignd );	

	double theta_z = input->zenith;
	if (theta_z > 86.0) theta_z = 86.0; // !Zenith angle must be < 90 (?? why 86?)
	if (theta_z < 0) theta_z = 0; // Zenith angle must be >= 0
	
	double W_spd = input->wspd;
	if (W_spd < 0.001) W_spd = 0.001;
	
	double eff_ref = modspec->Imp * modspec->Vmp / ( I_ref*modspec->Area );
	double tau_al = fabs(TauAlpha);
	if (G_total > 0)
		tau_al *= Geff_total/G_total;
			
	// TODO - shouldn't tau_al above include AM correction below?
	
	// !Calculation of Air Mass Modifier
	double air_mass = 1/(cos( theta_z*M_PI/180 )+0.5057*pow(96.080-theta_z, -1.634));
	// air_mass *= exp(-0.0001184 * Elevation); // optional correction for elevation (m), as applied in Sandia PV model
	double air_mass_modifier = a0 + a1*air_mass + a2*pow(air_mass,2) + a3*pow(air_mass,3) + a4*pow(air_mass,4);
	Geff_total *= air_mass_modifier;	
	
	double T_cell = celltemp + 273.15;
	if ( Geff_total >= 1.0 )
	{
		// calculation of IL and IO at operating conditions
		double IL = Geff_total/I_ref *( modspec->Il + muIsc*(T_cell-Tc_ref) );
		if (IL < 0.0) IL = 0.0;
		
		double EG = eg0 * (1-0.0002677*(T_cell-Tc_ref));
		double IO = modspec->Io * pow(T_cell/Tc_ref, 3) * exp( 1/KB*(eg0/Tc_ref - EG/T_cell) );
		double A = modspec->a * T_cell / Tc_ref;
		double Rsh = 1e6;
		if (Geff_total > 0.0)
			Rsh = modspec->Rsh*(I_ref/Geff_total);
			
		double V_oc = openvoltage_194( modspec->Voc, A, IL, IO, modspec->Rs, Rsh );
		double I_sc = IL/(1+modspec->Rs/Rsh);
		
		double P, V, I;
		
		if ( mode == MAXPOWERPOINT )
		{
			struct refparm refdata;
			refdata.a = A;
			refdata.Il = IL;
			refdata.Io = IO;
			refdata.Rs = modspec->Rs;
			refdata.Rsh = Rsh;

			P = -golden( 0, modspec->Voc, powerfunc, &refdata, 1e-4, &V);

			I = 0;
			if (V != 0) I=P/V;

			/*
			apd (aug2011)
  			 this is original code from the fortran impl (below)
			 revised code (above) using NR bracketing and golden method for minimization
			 yields over a 13x speed up
			  */

			  /*
			// maximum power
			P = 0;
			V = (modspec->Vmp + muVoc*(T_cell-Tc_ref))/2;
			I = 0;
			
			while( P <= V*I && V < V_oc )
			{
				P = V*I;
				V += 0.01;
				I = current_194( V, 0.9*IL, A, IL, IO, modspec->Rs, Rsh );
			}

*/
			
		}
		else
		{ // calculate power at specified operating voltage
			V = opvoltage;
			I = current_194( V, 0.9*IL, A, IL, IO, modspec->Rs, Rsh );
			P = V*I;
		}
		
//		printf("\tP=%lg V=%lg I=%lg\n", P, V, I);
		*power = P;
		*voltage  = V;
		*current = I;
		*voc = V_oc;
		*isc = I_sc;
		*eff = P/(modspec->Area*G_total);
	}

	return 0;
}
