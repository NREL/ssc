#include <stdio.h>
#include <math.h>
#include <cmath>
#include <limits>

#include "lib_cec6par.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279
#endif

#ifndef MAX
#define MAX(a,b) ( (a)>(b) ? (a) : (b) )
#endif

#ifndef MIN
#define MIN(a,b) ( (a)<(b) ? (a) : (b) )
#endif


#define sinD(x) sin((x)*0.017453292519943295769236907684886)
#define cosD(x) cos((x)*0.017453292519943295769236907684886)
#define tanD(x) tan((x)*0.017453292519943295769236907684886)
#define asinD(x) (57.295779513082320876798154814105*asin(x))
#define acosD(x) (57.295779513082320876798154814105*acos(x))
#define atanD(x) (57.295779513082320876798154814105*atan(x))

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


static double openvoltage_194( double Voc0, double a, double IL, double IO, double , double RSH )
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

static void transmittance( double incangdeg, /* incidence angle of incoming radiation (deg) */
		double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
		double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law */
		double l_thick, /* thickness of cover material (m), usually 2 mm for typical module */
		double *tr_surf,
		double *tr_cover ) 
{
	// reference: duffie & beckman, Ch 5.3
	
	double theta1 = incangdeg * M_PI/180.0;
	double theta2 = asin( 1.0 / n_cover * sin(theta1 ) ); // snell's law, assuming n_air = 1.0
	// fresnel's equation for non-reflected unpolarized radiation as 
	// an average of perpendicular and parallel components
	*tr_surf = 1 - 0.5 *
			( pow( sin(theta2-theta1), 2 )/pow( sin(theta2+theta1), 2)
			+ pow( tan(theta2-theta1), 2 )/pow( tan(theta2+theta1), 2 ) );
	
	*tr_cover = exp( -k * l_thick / cos(theta2) );
}

static void irradiance_through_cover(
	double theta,
	double theta_z,
	double tilt,
	double G_beam,
	double G_sky,
	double G_gnd,
	double *G_eff,
	double *S_abs
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
	double tr_norm_surf, tr_norm_cover;
	transmittance( 1.0, n_cover, k_trans, l_thick, &tr_norm_surf, &tr_norm_cover );
	double tau_norm = tr_norm_surf * tr_norm_cover;
	
	// transmittance of beam radiation, at incidence angle
	double tr_beam_surf, tr_beam_cover;
	transmittance( theta, n_cover, k_trans, l_thick, &tr_beam_surf, &tr_beam_cover );
	double tau_beam = tr_beam_surf * tr_beam_cover;

	// transmittance of sky diffuse, at modified angle by (D&B Eqn 5.4.2)
	double theta_sky = 59.7 - 0.1388*tilt + 0.001497*tilt*tilt;
	double tr_sky_surf, tr_sky_cover;
	transmittance( theta_sky, n_cover, k_trans, l_thick, &tr_sky_surf, &tr_sky_cover );
	double tau_sky = tr_sky_surf * tr_sky_cover;
	
	// transmittance of ground diffuse, at modified angle by (D&B Eqn 5.4.1)
	double theta_gnd = 90.0 - 0.5788*tilt  + 0.002693*tilt*tilt;
	double tr_gnd_surf, tr_gnd_cover;
	transmittance( theta_gnd, n_cover, k_trans, l_thick, &tr_gnd_surf, &tr_gnd_cover );
	double tau_gnd = tr_gnd_surf * tr_gnd_cover;

	// calculate component incidence angle modifiers, D&B Chap. 5 eqn 5.12.1, DeSoto'04
	double Kta_beam = tau_beam / tau_norm;
	double Kta_sky = tau_sky / tau_norm;
	double Kta_gnd = tau_gnd / tau_norm;
	
	// total effective irradiance absorbed by solar cell
	double Geff_total = G_beam*Kta_beam + G_sky*Kta_sky + G_gnd*Kta_gnd;

	if (Geff_total < 0) Geff_total = 0;
		
	*G_eff = Geff_total; // set output

	// total effective irradiance absorbed by cell and cover (used for energy balance in temp calcs)
	double shdkr = G_beam*tau_beam
		+ G_sky*tau_sky
		+ G_gnd*tau_gnd
		+ G_beam*( 1.0 - tr_beam_cover )
		+ G_sky*( 1.0 - tr_sky_cover )
		+ G_gnd*( 1.0 - tr_gnd_cover );
		
	*S_abs = shdkr; // set output
}


mcphys_celltemp_t::mcphys_celltemp_t( )
{
	mc = RACK;
	orient = NOIMPEDE;
	nrows = ncols = 1;
	module_width = module_length = W_gap = std::numeric_limits<double>::quiet_NaN();

}

// !*****************************************************************
static double free_convection( double TC, double TA, double SLOPE, double rho_air, 
	double Pr_air, double mu_air, double Area, double Length, double Width, double k_air)
{      
// !Function added by TN (2010)
// !Solution for free convection coefficienet as presented in Nellis and Klein (2008) and EES
	 
	double L_ch_f,nu,Beta,g_spec,Gr,Ra,C_lam,Nu_lam,C_turb,Nu_turb,Nu_bar,h_up,h_vert,h_down;
	static const double grav = 9.81;

	L_ch_f    = Area/(2.*(Length+Width));//  !Eq. 6-54 (Nellis&Klein)

	if (TA > TC) SLOPE = 180.0 - SLOPE;

	// !Properties Constant for Each Plate Scenario
	nu        = mu_air / rho_air; //          !Kinematic Viscosity
	Beta      = 1. / ((TA+TC)/2.); //         !volumetric coefficient of thermal expansion

	// !Horizontal Heated Upward Facing Plate (L_ch_f)
	// !OR Cooled Downward Facing Plate
	g_spec    = grav*max(0.,cosD(SLOPE));//  !Adjustment of gravity vector;
	Gr        = g_spec*Beta*fabs(TC-TA)*pow(L_ch_f,3)/pow(nu,2); //    !Grashof Number
	Ra        = max(0.0001,Gr*Pr_air); //                 !Rayleigh Number

	C_lam     = 0.671/pow(1. + pow(0.492/Pr_air, 9./16.) , 4./9.); //  !Eq. 6-49 (Nellis&Klein)
	Nu_lam    = 1.4/log(1.+(1.4/(0.835*C_lam*pow(Ra,0.25)))); // !Eq. 6-56 (Nellis&Klein)
	C_turb    = 0.14*((1.+0.0107*Pr_air)/(1.+0.01*Pr_air)); // !Eq. 6-58 (Nellis&Klein)
	Nu_turb   = C_turb*pow(Ra,1./3.); //        !Eq. 6-57  (Nellis&Klein)
	Nu_bar    = pow(pow(Nu_lam,10) + pow(Nu_turb,10.), (1./10.)); // !Eq. 6-55  (Nellis&Klein)
	h_up      = Nu_bar*k_air/L_ch_f;

	// !Vertical Plate (Length)
	g_spec    = grav*sinD(SLOPE);  //          !Adjustment of gravity vector
	Gr        = g_spec*Beta*fabs(TC-TA)*pow(Length,3)/pow(nu,2);        
	Ra        = max(0.0001,Gr*Pr_air);
	
	Nu_bar    = pow(0.825+(0.387*pow(Ra,(1./6.)))/pow(1+ pow(0.492/Pr_air, 9./16.), (8./27.)),2)  ;  // !(Incropera et al.,2006)
	h_vert    = Nu_bar*k_air/Length;  

	// !Horizontal Heated Downward Facing Plate
	// !OR Cooled Upward Facing Plate
	g_spec    = grav*max(0.,-cosD(SLOPE));
	Gr        = g_spec*Beta*fabs(TC-TA)*pow(L_ch_f,3)/pow(nu,2);
	Ra        = max(0.0001,Gr*Pr_air);

	Nu_bar    = 2.5/log(1.+(2.5/(0.527*pow(Ra,0.2)))*pow(1.+pow(1.9/Pr_air,0.9),2./9.));//    !Eq. 6-59  (Nellis&Klein)
	h_down    = Nu_bar*k_air/L_ch_f;

	// !Take Maximum of 3 Calculated Heat Transfer Coefficients

	return max(max(h_down,h_vert),h_up); //  !Fig. 6-12  (Nellis&Klein)
}

static double ffd( double D_h, double Re_dh )
{
	// !Function added by TN (2010)
	// !Solution for friction factor of channel flow as presented in Nellis and Klein (2008) and EES.

	static const double e = 0.005;
	return pow( (-2.*log10(max(1.e-6,((2.*e/(7.54*D_h)-5.02/Re_dh*log10(2.*e/(7.54*D_h)+13./Re_dh)))))), -2.0 );
}

static double channel_free( double W_gap, double SLOPE, double TA, double T_cr, double k_air,
	double rho_air, double cp_air, double mu_air, double Length )
{
	// !Function added by TN (2010)
	// !Solution for internal forced convection as presented in Nellis and Klein (2008) an d EES

	double g_spec,Beta,alpha,nu_air,Ra,Nu;

	static const double grav = 9.81;

	nu_air    = mu_air / rho_air; //          !Kinematic Viscosity 

	g_spec    = max(0.1, sinD(SLOPE)*grav);
	Beta   	= 1./((T_cr+TA)/2.);
	alpha 	= k_air / (rho_air * cp_air);
	Ra	    = max(0.001,g_spec*pow(W_gap,3)*Beta*(T_cr-TA)/(nu_air*alpha));
	Nu     	= Ra/24.*W_gap/Length*pow(1.0-exp(-35./Ra*Length/W_gap),0.75);

	return  Nu*k_air/W_gap;
}


bool mcphys_celltemp_t::operator() ( pvinput_t &input, pvpower_t &pwrfunc, double opvol, double *Tc )
{
	double err_P      = 100.0;  //!Set initial performance error. Must be > tolerance for power error in do loop
	double err_P1     = 100.0;  //!Set initial performance error for updated power guess
	double err_P2     = 0.0;    //!Set initial previous error.  Should be zero so approach factor doesn't reset after 1 iteration
	int p_iter     = 0;     //!Set iteration counter (performance)
	double app_fac_P  = 1.0;
        	
	double Geff_total, Shdkr;

	irradiance_through_cover(
			input.IncAng,
			input.Zenith,
			input.Tilt,
			input.Ibeam,
			input.Idiff,
			input.Ignd,
			
			&Geff_total,
			&Shdkr );		
	
	if (Geff_total < 1.0 )
	{	
		*Tc = input.Tdry;
		return true;
	}

	int mceff = mc;

	double Length = module_length;
	double Width = module_width;

	Length *= nrows;
	Width *= ncols;
	
	double Area = Length*Width;

	double L_char = 4.0 *Length*Width / (2*(Width + Length));

	if ( W_gap < 0.001 && mceff == GAP ) mceff = FLUSH;

	double R_gap = W_gap/(nrows*module_length);
	
	if ( mceff == GAP && R_gap > 1 && orient == VERTSUPP ) mceff = RACK;

	double TC = input.Tdry + 273.15;
	double P_atm = input.Patm;
	double V_cover = MAX(input.Wspd, 0.001);
	double TA = input.Tdry + 273.15;	
	double Fcg        = (1. - cosD(input.Tilt))/2; //   !view factor between top of tilted plate and horizontal plane adjacent to bottom edge of plate
	double Fcs        = 1. - Fcg; //              !view factor between top of tilted plate and everything else (sky)
	double Fbs        = Fcg; //                   !view factor bewteen top and ground = bottom and sky
	double Fbg        = Fcs; //                   !view factor bewteen bottom and ground = top and sky
	
	double AMASS  = 1/(cosD(input.Zenith)+0.5057*pow((96.080-input.Zenith),-1.634));
	double MAM    = a0+a1*AMASS+a2*pow(AMASS,2)+a3*pow(AMASS,3)+a4*pow(AMASS,4);
	double SunEff = MAM*(input.Ibeam + input.Idiff + input.Ignd);
	double EffRef = Pmp_ref/(1000*module_width*module_length);
	double P_guess = EffRef * (SunEff*Area);

	// sky temperature calculation, D&B 3.9.2  Tdew in 'C, TA and Tsky are Kelvin
	double Tsky = TA*(0.711+0.0056*input.Tdew+0.000073*pow(input.Tdew,2.0)+0.013*pow(cosD(input.TimeOfDayHr),0.25)); //   !Sky Temperature: Berdahl and Martin  
	double Tgnd = TA; // assume ground temperature is same as ambient
    
	double T_back = T_integ + 273.15;	
	double T_rw       = TA; //                    !Initial guess for roof or wall temp

	while ( fabs(err_P) > 0.1 )
	{
		double err_TC   = 100.0;  //!Set initial temperature error. Must be > tolerance for temp error in do loop
		double err_TC_p = 0.0;    //!Set initial previous error. Should be zero so approach factor doesn't reset after 1 iteration
		double app_fac  = 0.5;   //!Set approach factor for updating cell temp guess value
		double app_fac_v = 0.5;  //!Set approach factor for updating channel velocity guess value
 	    
		switch (mceff)
		{
		case RACK:
			{ // !Rack Mounting Configuration 
				int h_iter = 0;
				while ( fabs(err_TC) > 0.001 )
				{
					double rho_air    = P_atm*28.967/8314.34*(1./((TA+TC)/2.)); // !density of air as a function of pressure and ambient temp
					double Re_forced  = MAX(0.1,rho_air*V_cover*L_char/mu_air); //  !Reynolds number of wind moving across module
					double Nu_forced  = 0.037 * pow(Re_forced, (4./5.)) * pow(Pr_air,(1./3.)); //  !Nusselt Number (Incropera et al., 2006)
					double h_forced   = Nu_forced * k_air / L_char;
					double h_sky      = ( pow(TC,2)+pow(Tsky,2))*(TC+Tsky);
					double h_ground   = ( pow(TC,2)+pow(Tgnd,2))*(TC+Tgnd);
					double h_free_c   = free_convection(TC,TA, input.Tilt,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);  // !Call function to calculate free convection on tilted surface (top)           
					double h_free_b   = free_convection(TC,TA,180.-input.Tilt,rho_air,Pr_air,mu_air,Area,Length,Width,k_air); // !Call function to calculate free convection on tilted surface (bottom)              
					double h_conv_c   = pow( pow(h_forced,3.)+pow(h_free_c,3.), 1./3.); // !Combine free and forced heat transfer coefficients (top)
					double h_conv_b   = pow( pow(h_forced,3.)+pow(h_free_b,3.), 1./3.); // !Combine free and forced heat transfer coefficients (bottom)
            
					// !Energy balance to calculate TC
					double TC1 = ((h_conv_c+h_conv_b)*TA + (Fcs*EmisC+Fbs*EmisB)*sigma*h_sky*Tsky + (Fcg*EmisC+Fbg*EmisB)*sigma*h_ground*Tgnd
							- (P_guess/Area)+Shdkr)/(h_conv_c+h_conv_b+(Fcs*EmisC +Fbs*EmisB)*sigma*h_sky+(Fcg*EmisC+Fbg*EmisB)*sigma*h_ground);

					// !Since some variables in TC1 calc are function of TC, iterative solving is required        
					err_TC     = TC1 - TC; // !Error between n-1 and n temp calculations
					TC         = TC1; //      !Set cell temp to most recent calculation
                                
					if (h_iter++ > 150)
					{
						m_err = "cell temperature calculations did not converge";
						return false;
					}
				}
			}
			break;

	case FLUSH:
			{ // !Flush Mounting Configuration
				
				int h_iter = 0;
				while ( fabs(err_TC) > 0.001 )
				{
            
					double rho_air    = P_atm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and ambient temp
					double Re_forced  = MAX(0.1,rho_air*V_cover*L_char/mu_air); // !Reynolds number of wind moving across panel: function of L_char: array depen?
					double Nu_forced  = 0.037 * pow(Re_forced,(4./5.)) * pow(Pr_air,(1./3.));
					double h_forced   = Nu_forced * k_air / L_char;
					double h_sky      = ( pow(TC,2)+pow(Tsky,2))*(TC+Tsky);
					double h_ground   = ( pow(TC,2)+pow(Tgnd,2))*(TC+Tgnd);
					double h_free_c   = free_convection(TC,TA,input.Tilt,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);
					double h_conv_c   = pow( pow(h_forced,3.)+pow(h_free_c,3.), 1./3.);
                    
					double TC1 = ((h_conv_c)*TA + (Fcs*EmisC)*sigma*h_sky*Tsky + (Fcg*EmisC)*sigma*h_ground*Tgnd
							-(P_guess/Area)+Shdkr)/(h_conv_c+(Fcs*EmisC)*sigma*h_sky+(Fcg*EmisC)*sigma*h_ground);
                    
					err_TC     = TC1 - TC;
					TC         = TC1;
                                        
					if ( h_iter++ > 150 )
					{
						m_err = "cell temperature calculations did not converge";
						return false;
					}
				}
			}
			break;

		case INTEGRATED:
			{ // !Integrated Mounting Configuration
				int h_iter = 0;
				while (  fabs(err_TC) > 0.001 )
				{
					double rho_air    = P_atm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and film temp
					double rho_bk     = P_atm*28.967/8314.34*(1 / ((T_back+TC)/2.));
					double Re_forced  = MAX(0.1,rho_air*V_cover*L_char/mu_air); //  !Reynolds number of wind moving across panel: function of L_char: array depen?
					double Nu_forced  = 0.037 * pow(Re_forced,(4./5.)) * pow(Pr_air,(1./3.));
					double h_forced   = Nu_forced * k_air / L_char;
                    
					double h_sky      = ( pow(TC,2)+pow(Tsky,2))*(TC+Tsky);
					double h_ground   = ( pow(TC,2)+pow(Tgnd,2))*(TC+Tgnd);
					double h_radbk    = ( pow(TC,2)+pow(T_back,2))*(TC+T_back); //!Using T_back now instead of TA
                    
					double h_free_c   = free_convection(TC,TA,input.Tilt,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);
					double h_free_b   = free_convection(TC,T_back,180.-input.Tilt,rho_bk,Pr_air,mu_air,Area,Length,Width,k_air);
                 
					double h_conv_c   = pow( pow(h_forced,3.) + pow(h_free_c,3.),  1./3. );
					double h_conv_b   = h_free_b; // !No forced convection on backside
                    
					double TC1 = (h_conv_c*TA+h_conv_b*T_back+Fcs*EmisC*sigma*h_sky*Tsky+Fcg*EmisC*sigma*h_ground*Tgnd
							+EmisB*sigma*h_radbk*T_back-(P_guess/Area)+Shdkr)/(h_conv_c+h_conv_b+Fcs*EmisC*sigma*h_sky
							+Fcg*EmisC*sigma*h_ground+EmisB*sigma*h_radbk);
                    
					err_TC     = TC1 - TC;
					TC         = TC1;
                    
					if ( h_iter++ > 150 )
					{
						m_err = "cell temperature calculations did not converge";
						return false;
					}
				}
			}
			break;

		case GAP:
			{ // !Gap (channel) Mounting Configuration            

				double L_charB, L_str, A_c, Per_cw, D_h;

				if ( orient == NOIMPEDE )
				{
					//!Define channel length and width for gap mounting configuration that does not block air flow in any direction
					//!Use minimum dimension for length so that MSO 1 will have lower temp than MSO 2 or 3
					L_charB = min(Width, Length);
					L_str   = max(Width, Length);
                    
					//!These values are dependent on MSO
					A_c        = W_gap * L_str; //      !Cross Sectional area of channel
					Per_cw 	   = 2.*L_str; //           !Perimeter minus open sides
					D_h 	   = (4.*A_c)/Per_cw; //    !Hydraulic diameter
				}
				else if ( orient == VERTSUPP )
				{
					// !Vertical supports
					L_charB = Length;
					L_str   = Width / ncols;
					A_c        = W_gap * L_str; //          !Cross Sectional area of channel
					Per_cw 	   = 2.*L_str + 2.*W_gap; //    !Perimeter ACCOUNTING for supports: different than MSO 1
					D_h 	   = (4.*A_c)/Per_cw; //        !Hydraulic diameter
				}
				else if ( orient == HORIZSUPP )
				{
					//!Horizontal supports
					//!Flow is restricted to one direction.  Wind speed has already been adjusted using a cosine projection
					L_charB = Width;
					//!Width of channel is function of number of columns of modules.  Assuming that support structures are exactly the length of a module
					L_str   = Length / nrows ;
					A_c        = W_gap * L_str; //          !Cross Sectional area of channel
					Per_cw 	   = 2.*L_str + 2.*W_gap; //    !Perimeter ACCOUNTING for supports: different than MSO 1
					D_h 	   = (4.*A_c)/Per_cw  ; //      !Hydraulic diameter
				}
				else
				{
					m_err = "invalid gap mounting support orientation parameter";
					return false;
				}
                
				//!Begin iteration to find cell temperature             
				int h_iter = 0;
				while ( fabs(err_TC) > 0.001 )
				{
					double rho_air    = P_atm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and film temp
					double err_v      = 100; //                                     !set error for channel velocity iteration
					double err_v_p = 100;
					double P_in       = 0.5*pow(V_cover, 2) * rho_air; //  !Dynamic pressure at inlet
					double v_ch       = 0.3 * V_cover;
					// !Calculate air velocity through channel by assuming roughness and estimating pressure drop
					double v_iter     = 0 ;
					while ( v_iter++ < 80 && fabs(err_v) > 0.001 )
					{
						double Re_dh_ch   = rho_air*v_ch*(D_h / mu_air); //    !Reynolds number for channel flow
						double f_fd       = ffd(D_h,Re_dh_ch); //           !Friction factor of channel flow
						double tau_s      = f_fd*rho_air*pow(v_ch,2)/8.; //        !Shear stress on air
						double P_out      = P_in - tau_s*Per_cw*L_charB/A_c; // !Dynamic pressure at outlet
						double v_ch1      = pow(max(0.0005,(2*P_out/rho_air)), 1./2.); //  !velocity
						err_v      = v_ch1 - v_ch; //                   !Current Error
						double err_v_sign = err_v * err_v_p ; //               !Did error switch signs between previous calc?
						err_v_p    = err_v ; //                         !Previous Error     
                        
						if ( err_v_sign < 0. )
							app_fac_v = app_fac_v*0.5; //    !If error switched signs, reduced "approach factor"
                        
						v_ch       = v_ch + app_fac_v * err_v; //    !New velocity estimate equals old + a portion of the last error
					}

                
					// !Heat transfer on cover of module: using un-adjusted wind speed input
					double Re_forced  = MAX(0.1,rho_air*V_cover*L_char/mu_air);//  !Reynolds number of wind moving across panel
					double Nu_forced  = 0.037 * pow(Re_forced,(4./5.)) * pow(Pr_air,(1./3.));
					double h_forced   = Nu_forced * k_air / L_char;
					
					double h_sky      = ( pow(TC,2)+pow(Tsky,2))*(TC+Tsky);
					double h_ground   = ( pow(TC,2)+pow(Tgnd,2))*(TC+Tgnd);
					double h_free_c   = free_convection(TC,TA,input.Tilt,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);
					double h_conv_c   = pow( ( pow(h_forced,3.)+pow(h_free_c,3.)) , (1./3.) );
                
					//!Reynolds number for channel flow
					double Re_fp 	   = rho_air*v_ch*L_charB / mu_air;
					//!Use calculated channel velocity in flat plate correlation to find heat transfer coefficient
					//!This approach (rather than channel flow correlations) allows channel equations to approach open rack as gap increases
					double Nus_ch     = 0.037*pow(Re_fp,(4./5.))*pow(Pr_air,(1./3.));
					double h_ch       = Nus_ch * k_air / L_charB;
					h_ch       = min(h_ch, h_forced);//  !Make sure gap mounted doesn't calc lower temps than open rack
                    
					//!Set iteration  counter and initial error for roof/wall temperature iteration
					int iter_T_rw  = 0;
					double err_T_rw   = 2;

					double h_radbk = 0;
					double Q_conv_c = 0;
					double Q_conv_r = 0;
                
					//!Calculate roof temperature based on current cell temperature guess
					while ( iter_T_rw++ < 121 && fabs(err_T_rw) > 0.001)
					{
						double T_cr       = (TC+T_rw)/2.; // !Average of cell and roof temp assumed in correlations

						double h_fr;
						if (orient == HORIZSUPP) h_fr       = 0; // !If E-W supports then assume no free convection
						else h_fr       = channel_free(W_gap,input.Tilt,TA,T_cr,k_air,rho_air,cp_air,mu_air,Length);
             
						double m_dot  = v_ch*rho_air*A_c ; //!mass flow rate through channel
						double h_conv_b   = pow( pow(h_ch,3) + pow(h_fr,3), (1./3.)); //  !total heat transfer coefficient in channel
                        
						// !Calculate air temperature at the end of the channel 
						// !For MSO 2 & 3 have been calculating gap HT per channel(not necessarily entire array), so need to consider that going forward
						int AR = 1;
						if (orient == VERTSUPP) AR = ncols;
						else if (orient == HORIZSUPP) AR = nrows;
                
						double T_m = T_cr-(T_cr-TA)*exp(-2*(Area/AR)*h_conv_b/(m_dot*cp_air));
                     
						//!Using air temp at end of channel, calculate heat transfer to air in the channel: Then adjust for entire array (AR)
						double Q_air      = MAX(0.0001,cp_air*m_dot*(T_m - TA)) * AR;

						// !Determine the ratio of the heat transfered to channel that was from module and roof/wall by comparing temperatures
						double DELTAT_r   = T_rw - TA;
						double DELTAT_c   = TC - TA;
                        
						double R_r        = MIN(1., DELTAT_r / max(0.1,(DELTAT_r + DELTAT_c)));
						double R_c        = MIN(1., DELTAT_c / max(0.1,(DELTAT_r + DELTAT_c)));
                        
						// !Adjust to flux
						Q_conv_c   = R_c * Q_air / Area;
						Q_conv_r   = R_r * Q_air / Area;
                
						// !Calculate heat transfer coefficient for radiation
						h_radbk    = ( pow(TC,2) + pow(T_rw,2) )*(TC+T_rw);
						// !Energy Balance to calculate roof/wall temperature     
						double T_rw1        = MAX(TA, TC - Q_conv_r/(EmisB*sigma*h_radbk));

						err_T_rw    = T_rw1 - T_rw; //   !Error
						T_rw        = T_rw + (0.5-0.495*(iter_T_rw/60))*err_T_rw; //  !Reset guess
						
					}
                
					//!Once roof temperature has been solved for guess cell temperature, re-calculate cell temperature
					double TC1 = (h_conv_c*TA + Fcs*EmisC*sigma*h_sky*Tsky+Fcg*EmisC*sigma*h_ground*Tgnd+sigma*EmisB*h_radbk*T_rw
							- Q_conv_c-(P_guess/Area)+Shdkr)/(h_conv_c+Fcs*EmisC*sigma*h_sky+Fcg*EmisC*sigma*h_ground+sigma*EmisB*h_radbk);
                    
					err_TC      = TC1 - TC; //                  !Current Error
					double err_sign    = err_TC * err_TC_p; //         !Did error switch signs between previous calc?
					err_TC_p    = err_TC; //                    !Previous Error 
                    
					if(err_sign < 0.) app_fac = app_fac * 0.9;
                    
					TC         = TC + app_fac * err_TC;

					//!If cell temperature does not converge, give error message
					if( h_iter++ > 150 )
					{
						m_err = "invalid gap mounting support orientation parameter";
						return false;
					}                
				}
			}
			break;

		default:
			m_err = "invalid mounting configuration specification (mc)";
			return false; 
		}


		double Power, Voltage, Current, Eff, OpVoc, OpIsc;
		if ( !pwrfunc( input, TC-273.15, opvol, &Power, &Voltage, &Current, &Eff, &OpVoc, &OpIsc ) )
		{
			m_err = "iterative cell temp error in power calc: " + pwrfunc.error();
			return false;
		}

		err_P1     = Power - P_guess; // !Performance error
		double err_sign_P = err_P1 * err_P2;
		err_P2     = err_P1;
      
		if( p_iter > 5 && err_sign_P < 0.)
			app_fac_P = 0.75*app_fac_P;
      
		err_P      = (Power - P_guess); //    !Performance error for 1 panel
		P_guess    = P_guess + app_fac_P*err_P1; // !Set performance to most recent calc

      
		if ( p_iter++  > 300 )
		{
			m_err = "cell temp power iteration did not converge";
			return false;
		}
      
	} //   !End of power iteration which includes temperature calculations

	*Tc = TC-273.15;

	return true;
}


noct_celltemp_t::noct_celltemp_t( )
{
	Area = Vmp = Imp = Tnoct = std::numeric_limits<double>::quiet_NaN();
}

bool noct_celltemp_t::operator() ( pvinput_t &input, pvpower_t &, double opvol, double *Tc )
{
	double T_cell = input.Tdry + 273.15;
	
	double Geff_total, Shdkr;
	irradiance_through_cover(
		input.IncAng,
		input.Zenith,
		input.Tilt,
		input.Ibeam,
		input.Idiff,
		input.Ignd,
		
		&Geff_total,
		&Shdkr );		
	
	if (Geff_total > 1.0 )
	{	
		double G_total = input.Ibeam + input.Idiff + input.Ignd;		
		double eff_ref = Imp *Vmp / ( I_ref*Area );
		double tau_al = fabs(TauAlpha);
		double W_spd = input.Wspd;		
		if (W_spd < 0.001) W_spd = 0.001;		
		if (G_total > 0) tau_al *= Geff_total/G_total;		
		T_cell = (input.Tdry+273.15) + (G_total/I_noct * (Tnoct - Tamb_noct) * (1.0-eff_ref/tau_al))*9.5/(5.7 + 3.8*W_spd);
	}
	
	*Tc = T_cell - 273.15;

	return true;
}

struct refparm { double a, Il, Io, Rs, Rsh; };

static double powerfunc( double V, void *_d )
{
	struct refparm *r = (struct refparm*)_d;
	return -V*current_194( V, 0.9*r->Il, r->a, r->Il, r->Io, r->Rs, r->Rsh );
}

cec6par_power_t::cec6par_power_t( )
{
	Area = Vmp = Imp = Voc = Isc = alpha_isc = beta_voc = Tnoct
		= a = Il = Io = Rs = Rsh = Adj = std::numeric_limits<double>::quiet_NaN();
}

bool cec6par_power_t::operator() ( pvinput_t &input, double Tc, double opvoltage,
		double *Power, double *Voltage, double *Current,
		double *Eff, double *OpVoc, double *OpIsc )
{
	
	double muIsc = alpha_isc * (1-Adj/100);
	
	/* initialize output first */
	*Power = *Voltage = *Current = *Eff = *OpVoc = *OpIsc = 0.0;
	
	double G_total = input.Ibeam + input.Idiff + input.Ignd; // total incident irradiance on tilted surface, W/m2
		
	double Geff_total, Shdkr;
	irradiance_through_cover(
		input.IncAng,
		input.Zenith,
		input.Tilt,
		input.Ibeam,
		input.Idiff,
		input.Ignd,
		
		&Geff_total,
		&Shdkr );	

	double theta_z = input.Zenith;
	if (theta_z > 86.0) theta_z = 86.0; // !Zenith angle must be < 90 (?? why 86?)
	if (theta_z < 0) theta_z = 0; // Zenith angle must be >= 0
	
	double W_spd = input.Wspd;
	if (W_spd < 0.001) W_spd = 0.001;
	
	double tau_al = fabs(TauAlpha);
	if (G_total > 0)
		tau_al *= Geff_total/G_total;
			
	// TODO - shouldn't tau_al above include AM correction below?
	
	// !Calculation of Air Mass Modifier
	double air_mass = 1/(cos( theta_z*M_PI/180 )+0.5057*pow(96.080-theta_z, -1.634));
	air_mass *= exp(-0.0001184 * input.Elev); // correction for elevation (m), as applied in Sandia PV model
	double air_mass_modifier = a0 + a1*air_mass + a2*pow(air_mass,2) + a3*pow(air_mass,3) + a4*pow(air_mass,4);
	Geff_total *= air_mass_modifier;	
	
	double T_cell = Tc + 273.15;
	if ( Geff_total >= 1.0 )
	{
		// calculation of IL and IO at operating conditions
		double IL = Geff_total/I_ref *( Il + muIsc*(T_cell-Tc_ref) );
		if (IL < 0.0) IL = 0.0;
		
		double EG = eg0 * (1-0.0002677*(T_cell-Tc_ref));
		double IO = Io * pow(T_cell/Tc_ref, 3) * exp( 1/KB*(eg0/Tc_ref - EG/T_cell) );
		double A = a * T_cell / Tc_ref;
		double Rsh = 1e6;
		if (Geff_total > 0.0)
			Rsh = Rsh*(I_ref/Geff_total);
			
		double V_oc = openvoltage_194( Voc, A, IL, IO, Rs, Rsh );
		double I_sc = IL/(1+Rs/Rsh);
		
		double P, V, I;
		
		if ( opvoltage < 0 )
		{
			struct refparm refdata;
			refdata.a = A;
			refdata.Il = IL;
			refdata.Io = IO;
			refdata.Rs = Rs;
			refdata.Rsh = Rsh;

			P = -golden( 0, Voc, powerfunc, &refdata, 1e-4, &V);

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
			V = (Vmp + muVoc*(T_cell-Tc_ref))/2;
			I = 0;
			
			while( P <= V*I && V < V_oc )
			{
				P = V*I;
				V += 0.01;
				I = current_194( V, 0.9*IL, A, IL, IO, Rs, Rsh );
			}

*/
			
		}
		else
		{ // calculate power at specified operating voltage
			V = opvoltage;
			I = current_194( V, 0.9*IL, A, IL, IO, Rs, Rsh );
			P = V*I;
		}
		
		*Power = P;
		*Voltage  = V;
		*Current = I;
		*OpVoc = V_oc;
		*OpIsc = I_sc;
		*Eff = P/(Area*G_total);
	}

	return true;
}
