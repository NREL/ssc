#include <stdio.h>

#include <math.h>
#include "lib_cec5par.h"

#define min(a,b) ((a<b)?a:b)
#define max(a,b) ((a>b)?a:b)
#define sinD(x) sin(x*0.017453292519943295769236907684886)
#define cosD(x) cos(x*0.017453292519943295769236907684886)
#define tanD(x) tan(x*0.017453292519943295769236907684886)
#define asinD(x) (57.295779513082320876798154814105*asin(x))
#define acosD(x) (57.295779513082320876798154814105*acos(x))
#define atanD(x) (57.295779513082320876798154814105*atan(x))

// !*****************************************************************
static double free_convection_194( double TC, double TA, double SLOPE, double rho_air, 
	double Pr_air, double mu_air, double Area, double Length, double Width, double k_air)
{      
// !Function added by TN (2010)
// !Solution for free convection coefficienet as presented in Nellis and Klein (2008) and EES
	 
	double L_ch_f,nu,Beta,g_spec,Gr,Ra,C_lam,Nu_lam,C_turb,Nu_turb,Nu_bar,h_up,h_vert,h_down, v_ch;
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

static double ffd_194( double D_h, double Re_dh )
{
	// !Function added by TN (2010)
	// !Solution for friction factor of channel flow as presented in Nellis and Klein (2008) and EES.

	static const double e = 0.005;
	return pow( (-2.*log10(max(1.e-6,((2.*e/(7.54*D_h)-5.02/Re_dh*log10(2.*e/(7.54*D_h)+13./Re_dh)))))), -2.0 );
}

static double channel_free_194( double W_gap, double SLOPE, double TA, double T_cr, double k_air,
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

static const double KB = 8.618e-5; // Boltzmann constant [eV/K] note units
static const double a0 =0.918093, a1=0.086257, a2=-0.024459, a3=0.002816, a4=-0.000126; // !Air mass modifier coefficients as indicated in DeSoto paper
static const double T_prop=315.0, k_air=0.02676, mu_air=1.927E-5, Pr_air=0.724;  // !Viscosity in units of N-s/m^2
static const double EmisC = 0.84, EmisB = 0.7;  // Emissivities of glass cover, backside material
static const double sigma = 5.66961E-8, cp_air = 1005.5;

static const double n_cover = 1.526;   // !refractive index of glass
static const double l_thick = 0.002;   // !thickness of glass cover
static const double k_trans = 4; // proportionality constant for 

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
	double *Eff, double *Tcell, double *Geff
	)
{
	cec5par_module_t &m = *pModule;	
	
	double muIsc = m.mIsc * (1-m.Adjust/100);
	double muVoc = m.mVoc * (1-m.Adjust/100);
	
	T_amb += 273.15;  // convert to Kelvin from C	
	/* initialize output first */
	*Pmp = *Vmp = *Imp = *Voc = *Isc = *Eff = *Geff = 0.0;
	*Tcell = T_amb;
	
	double G_total = G_beam + G_sky + G_gnd; // total incident irradiance on tilted surface, W/m2
	
	double Tc0 = m.Tc0 + 273.15;
	
	
	/* ******** BEGIN CHECKS FOR ADVANCED TEMP MODEL *************** */
	
	/*
	
	int Nrows = m.NRows;
	int Ncols = m.NCols;
	
	if (m.HTD == 1)
		Nrows = Ncols = 1;
	
	double Length = m.Length;
	double Width = m.Width;
	
	if (m.HTD == 2)
	{
		Length = Nrows*m.Length;
		Width = Ncols*m.Width;
	}
	
	double Area_base = m.Area;	 // !Use provided area for Duffie and Beckman model to maintain consistency w/ previous model
	double Area = Length * Width; // !Surface area of module
    // !Define characteristic length
    double L_char     = 4.0 * Length * Width / (2.0 * (Width + Length));
    
    int MC = m.MC; // Mounting Configuration mode
    
    // !If gap is less than 1 mm, use flush mounting configuration
    if (m.Wgap < 0.001 && MC == 4) MC = 2;
    
	double R_gap = m.Wgap / Length;
	if ( MC == 4  && R_gap > 1 && m.MSO == 1) MC = 1;	
	
	*/
	
	/* ******** END CHECKS FOR ADVANCED TEMP MODEL *************** */
	
	
	
	
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
	
	//printf("\tGb=%lg Gs=%lg Gd=%lg\n", G_beam, G_sky, G_gnd);
	//printf("\tKta_beam=%lg Kta_sky=%lg Kta_gnd=%lg\n", Kta_beam, Kta_sky, Kta_gnd);
		
	// total effective irradiance absorbed by solar cell
	double Geff_total = G_beam*Kta_beam + G_sky*Kta_sky + G_gnd*Kta_gnd;
	
	if (Geff_total < 0) Geff_total = 0;
	
	
	double T_cell = T_amb;
	
	if (W_spd < 0.001) W_spd = 0.001;
	
	double eff_ref = m.Imp0 * m.Vmp0 / ( m.G0*m.Area );
	double tau_al = fabs(m.TauAlpha);
	if (G_total > 0)
		tau_al *= Geff_total/G_total;
			
	// TODO - shouldn't tau_al above include AM correction below?
	
	// !Calculation of Air Mass Modifier
	double air_mass = 1/(cos( theta_z*M_PI/180 )+0.5057*pow(96.080-theta_z, -1.634));
	// air_mass *= exp(-0.0001184 * Elevation); // optional correction for elevation (m), as applied in Sandia PV model
	double air_mass_modifier = a0 + a1*air_mass + a2*pow(air_mass,2) + a3*pow(air_mass,3) + a4*pow(air_mass,4);
	Geff_total *= air_mass_modifier;	
	
	if ( Geff_total >= 1.0 )
	{
	// calculation of cell temperature	
		T_cell = T_amb + (G_total/m.G_NOCT * (m.TcNOCT-m.TaNOCT) * (1.0-eff_ref/tau_al))*9.5/(5.7 + 3.8*W_spd);
		
		// calculation of IL and IO at operating conditions
		double IL = Geff_total/m.G0 *( m.IL0 + muIsc*(T_cell-Tc0) );
		if (IL < 0.0) IL = 0.0;
		
		double EG = m.Eg0 * (1-0.0002677*(T_cell-Tc0));
		double IO = m.IO0 * pow(T_cell/Tc0, 3) * exp( 1/KB*(m.Eg0/Tc0 - EG/T_cell) );
		double A = m.A0 * T_cell / Tc0;
		double Rsh = 1e6;
		if (Geff_total > 0.0)
			Rsh = m.Rsh0*(m.G0/Geff_total);
			
		double V_oc = openvoltage_194( m.Voc0, A, IL, IO, m.Rs, Rsh );
		double I_sc = IL/(1+m.Rs/Rsh);
		
		
		// maximum power
		double P_mp = 0;
		double V_mp = (m.Vmp0 + muVoc*(T_cell-Tc0))/2;
		double I_mp = 0;
		
		while( P_mp <= V_mp*I_mp && V_mp < V_oc )
		{
			P_mp = V_mp*I_mp;
			V_mp += 0.01;
			I_mp = current_194( V_mp, 0.9*IL, A, IL, IO, m.Rs, Rsh );
		}
		
		*Pmp = P_mp;
		*Vmp = V_mp;
		*Imp = I_mp;
		*Voc = V_oc;
		*Isc = I_sc;
		*Tcell = T_cell;
		*Eff = P_mp/(m.Area*G_total);
		*Geff = Geff_total;
	}

	/*
	double v_ch = 1.0, Fcg, Fcs, Fbs, Fbg, T_sky, T_ground, T_rw, TC;
	
	
	// !Set cover wind speed to wind input; backside wind speed may change based on mounting configuration
	double V_WIND = max(0.001,Wspd);
	double V_cover = V_WIND;
	double P_guess = 0;
	
	// !If temp models will be used, perform calcs that are only required once an iteration
	if ( SUNEFF >= 1.0 )
	{
		double EFFREF = 1e-3;
		
		// !Guess power based on SRC efficiency and irradiance
		if (MC == 5)
			EFFREF = m.ImR*m.VmR/(m.SunR*Area_base);   // !Efficiency of module at SRC conditions
		else
			EFFREF = m.ImR*m.VmR/(m.SunR*Area);   // !Efficiency of module at SRC conditions

		P_guess = EFFREF * (SUNEFF*Area); // !Estimate performance based on SRC efficiency
		if (m.HTD == 2) P_guess = P_guess * Nrows * Ncols;


		// !Adjust backside wind speed based on mounting structure orientation for "gap" mounting configuration
		if (MC == 4) {
			if (m.MSO==2) V_WIND  = max(0.001,fabs(cosD(Wdir-Azimuth))*V_WIND);
			if (m.MSO==3) V_WIND  = max(0.001,fabs(cosD(Wdir+90.-Azimuth))*V_WIND);
			v_ch = V_WIND * 0.3;   // !Give realistic starting value to channel air velocity
		}

		Fcg        = (1. - cosD(Slope))/2;  // !view factor between top of tilted plate and horizontal plane adjacent to bottom edge of plate
		Fcs        = 1. - Fcg;              // !view factor between top of tilted plate and everything else (sky)
		Fbs        = Fcg;                   // !view factor bewteen top and ground = bottom and sky
		Fbg        = Fcs;                   // !view factor bewteen bottom and ground = top and sky
		T_sky      = TA*pow(0.711+0.0056*Tdew+0.000073*pow(Tdew,2)+0.013*cosD(t_hr), 0.25);   // !Sky Temperature: Berdahl and Martin  
		T_ground   = TA;                    // !Set ground temp equal to ambient temp
		T_rw       = TA;                    // !Initial guess for roof or wall temp


		double ta_eff = (I_total > 0) ? (m.ta*I_eff_total/I_total) : m.ta;
		TC       = TA+(SunTilt*(m.TcNOCT-TaNOCT)/m.SunNOCT*(1.-EFFREF/ta_eff))*9.5/(5.7 + 3.8*V_cover);     // !Estimate cell temp w/ Duffie and Beckman equation

	}

	double err_P      = 100.;  // !Set initial performance error. Must be > tolerance for power error in do loop
	double err_P1     = 100.;  // !Set initial performance error for updated power guess
	double err_P2     = 0.;    // !Set initial previous error.  Should be zero so approach factor doesn't reset after 1 iteration
	int p_iter     = 0;     // !Set iteration counter (performance)
	double app_fac_P  = 1.;
	
	printf("entering iter, TC=%lg, MC=%d\n", TC, MC);

	while( p_iter <= 300 && fabs(err_P) > 0.1 )
	{
     	if (SUNEFF < 1)
     	{
			TC       = TA;
			printf("suneff  < 1, TC=TA %lg\n", TA);
		}
		else
		{			  
			double err_TC   = 100.; //  !Set initial temperature error. Must be > tolerance for temp error in do loop
			double err_TC_p = 0.; //    !Set initial previous error. Should be zero so approach factor doesn't reset after 1 iteration
			int h_iter   = 0 ; //    !Set iteration counter (temperature)
			double app_fac  = 0.5  ; //  !Set approach factor for updating cell temp guess value
			double app_fac_v = 0.5 ; // !Set approach factor for updating channel velocity guess value

			switch( MC )
			{
			case 1 : // !Rack Mounting Configuration 
				while( fabs(err_TC) > 0.001 )
				{

					double rho_air    = Patm*28.967/8314.34*(1./((TA+TC)/2.)) ; // !density of air as a function of pressure and ambient temp
					double Re_forced  = max(0.1,rho_air*V_cover*L_char/mu_air) ; //  !Reynolds number of wind moving across module
					double Nu_forced  = 0.037 * pow(Re_forced,4./5.) * pow(Pr_air, 1./3.) ; //  !Nusselt Number (Incropera et al., 2006)
					double h_forced   = Nu_forced * k_air / L_char;
					double h_sky      = (TC*TC+T_sky*T_sky)*(TC+T_sky);
					double h_ground   = (TC*TC+T_ground*T_ground)*(TC+T_ground);
					double h_free_c   = free_convection_194(TC,TA,Slope,rho_air,Pr_air,mu_air,Area,Length,Width,k_air) ; //   !Call function to calculate free convection on tilted surface (top)           
					double h_free_b   = free_convection_194(TC,TA,180.0-Slope,rho_air,Pr_air,mu_air,Area,Length,Width,k_air); // !Call function to calculate free convection on tilted surface (bottom)              
					double h_conv_c   = pow( pow(h_forced,3.) + pow(h_free_c,3.) , 1./3.) ; // !Combine free and forced heat transfer coefficients (top)
					double h_conv_b   = pow( pow(h_forced,3.) + pow(h_free_b,3.) , 1./3.) ; // !Combine free and forced heat transfer coefficients (bottom)
			
					// !Energy balance to calculate TC
					double TC1 = ( (h_conv_c+h_conv_b)*TA 
							+ (Fcs*EmisC+Fbs*EmisB)*sigma*h_sky*T_sky 
							+ (Fcg*EmisC+Fbg*EmisB)*sigma*h_ground*T_ground
							-(P_guess/Area)+SHDKR )
						 / ( h_conv_c 
							+ h_conv_b 
							+ (Fcs*EmisC +Fbs*EmisB)*sigma*h_sky 
							+ (Fcg*EmisC + Fbg*EmisB)*sigma*h_ground );

					// !Since some variables in TC1 calc are function of TC, iterative solving is required        
					err_TC     = TC1 - TC; // !Error between n-1 and n temp calculations
					TC         = TC1; //      !Set cell temp to most recent calculation
					
					h_iter++;			
					if ( h_iter > 150 ) return -1;
				}
				break;
				
			case 2: // !Flush Mounting Configuration
				while( fabs(err_TC) > 0.001)
				{
					double rho_air    = Patm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and ambient temp
					double Re_forced  = max(0.1,rho_air*V_cover*L_char/mu_air); //  !Reynolds number of wind moving across panel: function of L_char: array depen?
					double Nu_forced  = 0.037 * pow(Re_forced, 4./5.) * pow(Pr_air, 1./3.);
					double h_forced   = Nu_forced * k_air / L_char;
					double h_sky      = (TC*TC+T_sky*T_sky)*(TC+T_sky);
					double h_ground   = (TC*TC+T_ground*T_ground)*(TC+T_ground);
					double h_free_c   = free_convection_194(TC,TA,Slope,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);
					double h_conv_c   = pow((pow(h_forced,3.) + pow(h_free_c,3.)), (1./3.));
					
					double TC1 = ((h_conv_c)*TA + (Fcs*EmisC)*sigma*h_sky*T_sky + (Fcg*EmisC)*sigma*h_ground*T_ground
						 - (P_guess/Area)+SHDKR)/(h_conv_c+(Fcs*EmisC)*sigma*h_sky+(Fcg*EmisC)*sigma*h_ground);
					
					err_TC     = TC1 - TC;
					TC         = TC1;
					
					h_iter++;
					if (h_iter > 150) return -1;
				}
				break;
			
			case 3: // !Integrated Mounting Configuration
				while( fabs(err_TC) > 0.001)
				{
			
					double rho_air    = Patm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and film temp
					double rho_bk     = Patm*28.967/8314.34*(1 / ((TbackInteg+TC)/2.));
					double Re_forced  = max(0.1,rho_air*V_cover*L_char/mu_air); //  !Reynolds number of wind moving across panel: function of L_char: array depen?
					double Nu_forced  = 0.037 * pow(Re_forced, 4./5.) * pow(Pr_air, 1./3.);
					double h_forced   = Nu_forced * k_air / L_char;					
					double h_sky      = (TC*TC+T_sky*T_sky)*(TC+T_sky);
					double h_ground   = (TC*TC+T_ground*T_ground)*(TC+T_ground);				   
					double h_radbk    = (TC*TC+TbackInteg*TbackInteg)*(TC+TbackInteg); // !Using TbackInteg now instead of TA					
					double h_free_c   = free_convection_194(TC,TA,Slope,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);				 
					double h_free_b   = free_convection_194(TC,TbackInteg,180.-Slope,rho_bk,Pr_air,mu_air,Area,Length,Width,k_air);				 
					double h_conv_c   = pow( pow(h_forced,3.) + pow(h_free_c,3.), (1./3.));
					double h_conv_b   = h_free_b;// !No forced convection on backside
					
					double TC1 = (h_conv_c*TA+h_conv_b*TbackInteg+Fcs*EmisC*sigma*h_sky*T_sky+Fcg*EmisC*sigma*h_ground*T_ground
									+EmisB*sigma*h_radbk*TbackInteg-(P_guess/Area)+SHDKR)
								/ (h_conv_c+h_conv_b+Fcs*EmisC*sigma*h_sky+Fcg*EmisC*sigma*h_ground+EmisB*sigma*h_radbk);
					
					err_TC     = TC1 - TC;
					TC         = TC1;
					
					h_iter++;
					if (h_iter > 150) return -1;
				}
				break;
				
			case 4: // !Gap (channel) Mounting Configuration
				{
					double A_c, L_charB, L_str, Per_cw, D_h;
				
					if ( m.MSO == 1)
					{
						// !Define channel length and width for gap mounting configuration that does not block air flow in any direction
						// !Use minimum dimension for length so that MSO 1 will have lower temp than MSO 2 or 3
						L_charB = min(Width, Length);
						L_str   = max(Width, Length);
						
						// !These values are dependent on MSO
						A_c        = m.Wgap * L_str;     // !Cross Sectional area of channel
						Per_cw 	   = 2.*L_str;          // !Perimeter minus open sides
						D_h 	   = (4.*A_c)/Per_cw;   // !Hydraulic diameter
					}
					else if (m.MSO == 2) //  !Vertical supports
					{
						L_charB = Length;
						L_str   = Width / Ncols;
						A_c        = m.Wgap * L_str ; //         !Cross Sectional area of channel
						Per_cw 	   = 2.*L_str + 2.*m.Wgap ; //   !Perimeter ACCOUNTING for supports: different than MSO 1
						D_h 	   = (4.*A_c)/Per_cw ; //       !Hydraulic diameter
					}
					else if (m.MSO == 3) // ! Horizontal supports
					{
						// !Flow is restricted to one direction.  Wind speed has already been adjusted using a cosine projection
						L_charB = Width;
						// !Width of channel is function of number of columns of modules.  Assuming that support structures are exactly the length of a module
						L_str   = Length / Nrows;
						A_c        = m.Wgap * L_str ; //         !Cross Sectional area of channel
						Per_cw 	   = 2.*L_str + 2.*m.Wgap ; //   !Perimeter ACCOUNTING for supports: different than MSO 1
						D_h 	   = (4.*A_c)/Per_cw ; //       !Hydraulic diameter
					}
					else
						return -2; // invalid parameter specified
					
					// !Begin iteration to find cell temperature
					while ( fabs(err_TC) > 0.001 )
					{      
						double rho_air    = Patm*28.967/8314.34*(1 / ((TA+TC)/2.)); // !density of air a function of pressure and film temp
						double err_v      = 100; //                                     !set error for channel velocity iteration
						double err_v_p    = 100;
						double P_in       = 0.5*V_WIND*V_WIND * rho_air; //  !Dynamic pressure at inlet
						int v_iter     = 0 ; //                       !set iteration counter for channel velocity iteration
						
						// !Calculate air velocity through channel by assuming roughness and estimating pressure drop
						while ( v_iter < 80 && fabs(err_v) > 0.001)
						{
							double Re_dh_ch   = rho_air*v_ch*(D_h / mu_air) ; //   !Reynolds number for channel flow
							double f_fd       = ffd_194(D_h,Re_dh_ch)  ; //         !Friction factor of channel flow
							double tau_s      = f_fd*rho_air*pow(v_ch,2/8.); //        !Shear stress on air
							double P_out      = P_in - tau_s*Per_cw*L_charB/A_c; // !Dynamic pressure at outlet
							double v_ch1      = sqrt(max(0.0005,(2*P_out/rho_air))); //  !velocity
							err_v      = v_ch1 - v_ch; //                   !Current Error
							double err_v_sign = err_v * err_v_p; //                !Did error switch signs between previous calc?
							err_v_p    = err_v; //                          !Previous Error     
							
							if(err_v_sign < 0.) app_fac_v=app_fac_v*0.5; //    !If error switched signs, reduced "approach factor"
							
							v_ch       = v_ch + app_fac_v * err_v; //         !New velocity estimate equals old + a portion of the last error
							v_iter++ ; //                    !Add 1 to iteration counter
						}

					
						// !Heat transfer on cover of module: using un-adjusted wind speed input
						double Re_forced  = max(0.1,rho_air*V_cover*L_char/mu_air); //  !Reynolds number of wind moving across panel
						double Nu_forced  = 0.037*pow(Re_forced,4./5.)*pow(Pr_air,1./3.);
						double h_forced   = Nu_forced * k_air / L_char;
						double h_sky      = (TC*TC+T_sky*T_sky)*(TC+T_sky);
						double h_ground   = (TC*TC+T_ground*T_ground)*(TC+T_ground);
						double h_free_c   = free_convection_194(TC,TA,Slope,rho_air,Pr_air,mu_air,Area,Length,Width,k_air);
						double h_conv_c   = pow(pow(h_forced,3.) + pow(h_free_c,3.), 1./3.);
					
						// !Reynolds number for channel flow
						double Re_fp 	   = rho_air*v_ch*L_charB / mu_air;
						// !Use calculated channel velocity in flat plate correlation to find heat transfer coefficient
						// !This approach (rather than channel flow correlations) allows channel equations to approach open rack as gap increases
						double Nus_ch     = 0.037*pow(Re_fp,4./5.)*pow(Pr_air,1./3.);
						double h_ch       = Nus_ch * k_air / L_charB;
						h_ch       = min(h_ch, h_forced); //           !Make sure gap mounted doesn't calc lower temps than open rack
						
						// !Set iteration  counter and initial error for roof/wall temperature iteration
						int iter_T_rw  = 0;
						double err_T_rw   = 2;
					
						double h_radbk = 0;
						double Q_conv_c = 0, Q_conv_r = 0;
						// !Calculate roof temperature based on current cell temperature guess
						while ( iter_T_rw < 121 && fabs(err_T_rw) > 0.001 )
						{
							double T_cr = (TC+T_rw)/2.; // !Average of cell and roof temp assumed in correlations

							double h_fr = 0;
							
							if (m.MSO == 3) h_fr = 0; //  !If E-W supports then assume no free convection
							else h_fr = channel_free_194(m.Wgap,Slope,TA,T_cr,k_air,rho_air,cp_air,mu_air,Length); // !Call function for channel free convection        
				 
							double m_dot 	   = v_ch*rho_air*A_c ; // !mass flow rate through channel
							double h_conv_b   = pow( pow(h_ch,3) + pow(h_fr,3) , (1./3.)) ; // !total heat transfer coefficient in channel
							
							// !Calculate air temperature at the end of the channel 
							// !For MSO 2 & 3 have been calculating gap HT per channel(not necessarily entire array), so need to consider that going forward
							
							int AR = 0;
							if (m.MSO == 1) AR = 1;
							if (m.MSO == 2) AR = Ncols;
							if (m.MSO == 3) AR = Nrows;
					
							double T_m = T_cr-(T_cr-TA)*exp(-2*(Area/AR)*h_conv_b/(m_dot*cp_air));
						 
							// !Using air temp at end of channel, calculate heat transfer to air in the channel: Then adjust for entire array (AR)
							double Q_air = max(0.0001, cp_air*m_dot*(T_m - TA)) * AR;

							// !Determine the ratio of the heat transfered to channel that was from module and roof/wall by comparing temperatures
							double DELTAT_r   = T_rw - TA;
							double DELTAT_c   = TC - TA;
							
							double R_r        = min(1., DELTAT_r / max(0.1,(DELTAT_r + DELTAT_c)));
							double R_c        = min(1., DELTAT_c / max(0.1,(DELTAT_r + DELTAT_c)));
							
							// !Adjust to flux
							Q_conv_c   = R_c * Q_air / Area;
							Q_conv_r   = R_r * Q_air / Area;
					
							// !Calculate heat transfer coefficient for radiation
							h_radbk    = (TC*TC+T_rw*T_rw)*(TC+T_rw);
							// !Energy Balance to calculate roof/wall temperature     
							double T_rw1        = max(TA, TC - Q_conv_r/(EmisB*sigma*h_radbk));

							err_T_rw    = T_rw1 - T_rw; //   !Error
							T_rw        = T_rw + (0.5-0.495*(iter_T_rw/60))*err_T_rw; //  !Reset guess
							iter_T_rw++; //   !Increase iteration counter
						}
					
						// Once roof temperature has been solved for guess cell temperature, re-calculate cell temperature
						double TC1 = (h_conv_c*TA + Fcs*EmisC*sigma*h_sky*T_sky+Fcg*EmisC*sigma*h_ground*T_ground+sigma*EmisB*h_radbk*T_rw
							 -Q_conv_c-(P_guess/Area)+SHDKR)/(h_conv_c+Fcs*EmisC*sigma*h_sky+Fcg*EmisC*sigma*h_ground+sigma*EmisB*h_radbk);
						
						err_TC      = TC1 - TC ; //                  !Current Error
						double err_sign    = err_TC * err_TC_p ; //        !Did error switch signs between previous calc?
						err_TC_p    = err_TC ; //                   !Previous Error 
						
						if(err_sign < 0.) app_fac = app_fac * 0.9;
						
						TC         = TC + app_fac * err_TC;

						h_iter++;
						
						if (h_iter > 150) return -1;
					}
				}
				break;
				
			case 5:
				TC = TC; //  !Use Duffie and Beckman temperature model  
				break;
			}
		}
		  
		  
		// !Evaluation of IL and IO at operating conditions, for one module
		double IL   = (SUNEFF/m.SunR)*(m.ILRef+m.mIsc*(TC-m.TcR));
		
		
		printf("eval TC=%lg, IL=%lg\n", TC, IL);

		if (IL < 0.0) IL=0.0;
		double EG   = m.EgRef*(1-0.0002677*(TC-m.TcR));
		double IO   = m.IORef*(pow(TC/m.TcR, 3))*exp((1/KB)*(m.EgRef/m.TcR-EG/TC));
		double A    = m.ARef*TC/m.TcR;
		double RSH  = 1E6;
		
		if (SUNEFF > 0.0)
		{
			RSH = m.RshRef*(m.SunR/SUNEFF); // !Shunt resistance relation in DeSoto paper
		}

		// !Open circuit voltage, short circuit current for 1 module
		double VOC = openvoltage_194(m.VocR,SUNEFF, A, IL, IO, m.RS, RSH);
		double ISC = IL/(1+m.RS/RSH);
		
		printf("eval Voc=%lg, Isc=%lg\n", VOC, ISC);

			  
		// !****   all calculations are being skipped during time periods
		// !****   with no insolation
		

		double V    = 0.0;
		double I    = 0.0;
		double UTIL = 0.0;
		double FF   = 0.0;

		if (SUNEFF > 0)
		{
			// !**** check on operation mode
			if (V < 0.) {
				V=0.;
				I=0.;
			} else {
				// !****normal operation
				// !****check if voltage greater than open circuit voltage
				if((V) > VOC) {
					V=VOC;
					I=0.;
				} else {
				//static double current_194( double V, double IMR, double A, double IL, double IO, double RS, double RSH )
					I=current_194(V,0.9*IL, A, IL, IO, m.RS, RSH);
				}
			}

	// Label 1000    
			double P=I*V; //	!POWER FOR ENTIRE ARRAY

			// !MAXIMUM POWER FOR THE ARRAY
			PMAX=0;
			VMP=0;
			IMP=0;
			VMP=(m.VmR+m.mVoc*(TC-m.TcR))/2;
			while (PMAX <= IMP*VMP && VMP < VOC)
			{
				PMAX=IMP*VMP;
				VMP=VMP+0.01;
				IMP=current_194(VMP,0.9*IL, A, IL, IO, m.RS, RSH);//  !IMP PER MODULE 
			}
			  
			PMAX = IMP*VMP; //  !PER ARRAY

			if(PMAX != 0.) UTIL=P/PMAX;
			else UTIL=0.;

			// !Fill factor
			if ((VOC > 0.0) && (ISC > 0.0))
				FF=VMP*IMP/VOC/ISC;
			else
				FF=0;
		}
		  
		if ( (SUNEFF < 1) || (MC == 5)) break; // !Don't need to iterate if temp is not a function of power
		  
		double PMAX_1    = m.Derate * PMAX; // !Calculate power per module  3/22/11: Multiply by module level derate
		  
		if (m.HTD == 2)   PMAX_1    = PMAX_1 * Nrows * Ncols; //   !Calculate power on # modules used for heat transfer calcs
		 
		err_P1     = PMAX_1 - P_guess; // !Performance error
		double err_sign_P = err_P1 * err_P2;
		err_P2     = err_P1;
		  
		if ((p_iter > 5) && (err_sign_P < 0.)) app_fac_P = 0.75*app_fac_P;
		  
		err_P      = (PMAX_1 - P_guess)/(Nrows * Ncols); //    !Performance error for 1 panel
		P_guess    = P_guess + app_fac_P*err_P1; // !Set performance to most recent calc
		p_iter     = p_iter + 1; // !+1 to iteration counter
		  
		if (p_iter > 300) return -1;		  
	} // !End of power iteration which includes temperature calculations
   
	// set outputs
	*Pmp = PMAX;
	*Vmp = VMP;
	*Imp = IMP;
	*Eff = PMAX/(Area*SunTilt);
	*Tcell = TC;
	
	*/
	
}
