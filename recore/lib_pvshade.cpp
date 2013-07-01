#include "lib_pvshade.h"
#include <math.h>
#include <limits>
#include <sstream>
#include <vector>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif
#ifndef M_EPS
#define M_EPS 0.00001
#endif

#define sind(x) sin( (M_PI/180.0)*(x) )
#define cosd(x) cos( (M_PI/180.0)*(x) )
#define tand(x) tan( (M_PI/180.8)*(x) )
#define max(a,b) (((a) > (b))?(a):(b))
#define min(a,b) (((a) < (b))?(a):(b))

double atand( double radians )
{
	// return angle in radians (-pi/2, pi/2) or in degrees (-90,90)
	double arctan_val = atan( radians );
	while ( arctan_val > M_PI/2.0) arctan_val -= M_PI;
	while ( arctan_val < -M_PI/2.0) arctan_val += M_PI;
	return (180.0/ M_PI) * arctan_val; //convert angle from radians to degrees
}


double round(double number)
{
    return number < 0.0 ? ceil(number - 0.5) : floor(number + 0.5);
}



// mask angle function for integration from documentation\PV\Shading\ChrisDeline\2012.4.23\SAM shade geometry_v2.docx
double mask_angle_func(double x, double R, double B, double tilt_eff)
{
	return atan2( (B-x) * sind(tilt_eff), (R-B*cosd(tilt_eff) + x*cosd(tilt_eff)) );
}



// Romberg integration for phi_bar from Numerical Recipes in C
#define EPS 1.0e-6
#define JMAX 20
#define JMAXP (JMAX+1)
#define K 5
#define FUNC(x,R,B,tilt) ((*func)(x,R,B,tilt))

double trapzd(double (*func)(double,double,double,double), double a, double b, double R, double B, double tilt, int n)
{
	double x,tnm,sum,del;
	static double s;
	int it,j;
	if (n == 1) 
	{
		return (s=0.5*(b-a)*(FUNC(a,R,B,tilt)+FUNC(b,R,B,tilt)));
	} 
	else 
	{
		for (it=1,j=1;j<n-1;j++) it <<= 1;
		tnm=it;
		del=(b-a)/tnm; /*This is the spacing of points to be added. */
		x=a+0.5*del;
		for (sum=0.0,j=1;j<=it;j++,x+=del) sum += FUNC(x,R,B,tilt);
		s=0.5*(s+(b-a)*sum/tnm); /*This replaces s by its refined value.*/
		return s;
	}
}
/* ********************************************************************* */
void polint(double xa[], double ya[], int n, double x, double *y, double *dy)
{
	int i,m,ns=1;
	double den,dif,dift,ho,hp,w;
//	double *c,*d;
//	c=vector(1,n);
//	d=vector(1,n);
	size_t size = n+1;	
	std::vector<double> c(size);
	std::vector<double> d(size);
	dif=fabs(x-xa[1]);

	for (i=1;i<=n;i++) 
	{
		if ( (dift=fabs(x-xa[i])) < dif) 
		{
			ns=i;
			dif=dift;
		}
		c[i]=ya[i];
		d[i]=ya[i];
	}
	*y=ya[ns--];
	for (m=1;m<n;m++) 
	{
		for (i=1;i<=n-m;i++) 
		{
			ho=xa[i]-x;
			hp=xa[i+m]-x;
			w=c[i+1]-d[i];
			den = ho-hp;
			//if ( (den=ho-hp) == 0.0) nrerror("Error in routine polint");
			if (den != 0) den=w/den;
			d[i]=hp*den;
			c[i]=ho*den;
		}
		*y += (*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
	}
//free_vector(d,1,n);
//free_vector(c,1,n);
}
/* ********************************************************************* */
double qromb(double (*func)(double,double,double,double), double a, double b, double R, double B, double tilt)
{
	void polint(double xa[], double ya[], int n, double x, double *y, double *dy);
	double trapzd(double (*func)(double,double,double,double), double a, double b, double R, double B, double tilt, int n);
	void nrerror(char error_text[]);
	double ss,dss;
	double s[JMAXP],h[JMAXP+1];
	int j;
	h[1]=1.0;
	for (j=1;j<=JMAX;j++) 
	{
		s[j]=trapzd(func,a,b,R,B,tilt,j);
		if (j >= K) 
		{
			polint(&h[j-K],&s[j-K],K,0.0,&ss,&dss);
			if (fabs(dss) <= EPS*fabs(ss)) return ss;
		}
		h[j+1]=0.25*h[j];
	}
	//nrerror("Too many steps in routine qromb");
	return 0.0;
}
// end of Romberg integration functions




selfshade_t::selfshade_t()
{
}


selfshade_t::selfshade_t( ssarrdat &arr )
{
	m_arr = arr;
	init();
}

void selfshade_t::init()
{
	if (m_arr.mod_orient == 0) //            ! Portrait Mode
		m_A = m_arr.length * m_arr.nmody;
	else //                                   ! Landscape Mode
		m_A = m_arr.width * m_arr.nmody;

	m_tilt_eff = m_arr.tilt - m_arr.slope_ns;

	m_m = m_arr.nmody;
	m_n = m_arr.nmodx;
	m_d = m_arr.ndiode;
	m_W = m_arr.width;
	m_L = m_arr.length;
	m_r = m_arr.nrows;
	m_R = m_arr.row_space;
	m_B = 0.0;

	// check for divide by zero issues with Row spacing per email from Chris 5/2/12
	if (m_R < M_EPS) m_R = M_EPS;


	if ( m_arr.mod_orient == 0 ) // Portrait mode
	{
		m_B = m_L*m_m;
	}
	else
	{
		m_B = m_W*m_m;
	}

	double a = 0.0, b = m_B;

	double mask_angle;

	if ( m_arr.mask_angle_calc_method == 1 )
	{
	// average over entire array
		mask_angle = qromb( mask_angle_func, a, b, m_R, m_B, m_tilt_eff) / m_B;
	}
	else
	{
	// worst case (default)
	// updated to phi(0) per email from Chris Deline 5/2/12
		mask_angle = atan2( ( m_B * sind( m_tilt_eff ) ), ( m_R - m_B * cosd( m_tilt_eff ) ) );
	}

	mask_angle *= 180.0/M_PI; 

	m_mask_angle = mask_angle; // report in degrees

}

void diffuse_reduce( 
		// inputs (angles in degrees)
		double solzen,
		double stilt,
		double Gb_nor,
		double Gd_poa,
		double gcr,
		double phi0, // mask angle
		double alb,
		double nrows,

		// outputs
		double &reduced_skydiff,
		double &Fskydiff,  // derate factor on sky diffuse
		double &reduced_gnddiff,
		double &Fgnddiff ) // derate factor on ground diffuse
{
	if ( Gd_poa < 0.1 ) 
	{
		Fskydiff = Fgnddiff = 1.0;
		return;
	}

	// view factor calculations assume isotropic sky
	double Gd = Gd_poa; // total plane-of-array diffuse
	double Gdh = Gd * 2/(1+cosd(stilt)); // total
	double Gbh = Gb_nor * cosd( solzen ); // beam irradiance on horizontal surface
						
	// sky diffuse reduction
	reduced_skydiff = Gd - Gdh*(1 - pow( cosd(phi0/2), 2 ))*( nrows - 1.0) / nrows;
	Fskydiff = reduced_skydiff / Gd;
						
	double B = 1.0;
	double R = B/gcr;

	double solalt = 90-solzen;

	// ground reflected reduction 
	double F1 = alb * pow( sind(stilt/2.0), 2);
	double Y1 = R - B * sind( 180.0 - solalt - stilt ) / sind(solalt);
	Y1 = max(0.00001, Y1); // constraint per Chris 4/23/12
	double F2 = 0.5 * alb * ( 1.0 + Y1/B - sqrt( pow(Y1,2)/pow(B,2) - 2*Y1/B * cosd(180 - stilt) + 1.0 ) );
	double F3 = 0.5 * alb * ( 1.0 + R/B - sqrt( pow(R,2)/pow(B,2) - 2*R/B * cosd(180 - stilt) + 1.0 ) );
						
	double Gr1 = F1 * (Gbh + Gdh);
	reduced_gnddiff = ( (F1 + (nrows-1)*F2)/ nrows ) * Gbh
		+ ( (F1 + (nrows-1) * F3)/ nrows ) * Gdh;

	Fgnddiff = 1.0;
	if ( Gr1 > 0 )
		Fgnddiff = reduced_gnddiff / Gr1;
}

double selfshade_dc_derate( double X, double S, double FF0, double dbh_ratio )
{
	double Xtemp = min( X , 0.65);  // X is limited to 0.65 for c2 calculation
	
	double c1 = (109 * FF0 - 54.3) * exp(-4.5 * X); // new c1 on 1/18/13
	double c2 = -6 * pow(Xtemp,2) + 5 * Xtemp + 0.28; // new c2 on 1/18/13
	double c3_0 = (-0.05 * dbh_ratio - 0.01) * X + (0.85 * FF0 - 0.7) * dbh_ratio - 0.085 * FF0 + 0.05;  //new c3_0 on 1/18/13
	double c3 = max ( c3_0, ( dbh_ratio ) - 1.0 );
	double eqn5 = 1.0 - c1 * pow(S,2) - c2 * S;  // new eqn5 on 1/18/13
	double eqn9 = 0;

	if ( X != 0 ) eqn9 = (X - S) / X;

	double eqn10 = c3 * ( S - 1.0 ) + ( dbh_ratio );

	double reduc = max( eqn5, eqn9);
	reduc = max( reduc, eqn10 );
	reduc = X * reduc + (1.0 - X);

	// check limits
	if (reduc > 1) reduc = 1.0;
	if (reduc < 0) reduc = 0.0;

	return reduc;
}

void selfshade_xs_horstr( bool landscape, 
						   double W,   // module width (short side)
						   double L,   // module length (long side)
						   int r,      // number of rows
						   int m,      // number of modules along row edge (short side of assembly)
						   int n,      // number of modules along (long side of assembly)
						   int ndiode, // number of bypass diodes
						   double Fshad, // Fraction of assembly shaded up from long edge

						   // outputs (pass by reference)
						   double &X, double &S)
{
	// if number of modules across bottom > number in string and horizontal wiring then g=0
	double g = 0; // assume very long rows

	if ( landscape ) // Landscape mode
	{
		double Hs = Fshad * m * W;


		if ( Hs <= W )
		{ // Situation 1a
			X = ( ceil( Hs / W ) / (m * r) ) * ( r - 1.0 );
			// updated to more conservative approach - email from Chris 4/10/12
			S = ( ceil( Hs * ndiode / W ) / ndiode ) * ( 1.0 - floor( g / L ) / n);
		}
		else // Hs > m_arr.width
		{  // Situation 1b
			X = ( ceil( Hs / W ) / (m * r) ) * ( r - 1.0);
			S = 1.0;
		}
	}
	else // Portrait mode
	{  // Situation 2
		double Hs = Fshad * m * L;

		X = ( ceil( Hs / L ) / (m * r) ) * ( r - 1.0);
		S = 1.0 - ( floor( g * ndiode / W ) / ( ndiode * n) );
	}
}

bool selfshade_t::exec(
		double solzen,
		double solazi,
		double Gb_nor,
		double Gb_poa,
		double Gd_poa,
		double FF0,
		double albedo)
{
/*
Chris Deline 4/9/2012 - updated 4/19/2012 - update 4/23/2012 
see SAM shade geometry_v2.docx
Updated 1/18/13 to match new published coefficients in Solar Energy "A simplified model of uniform shading in large photovoltaic arrays"

Definitions of X and S in SAM for the four layout conditions – portrait, landscape and vertical / horizontal strings.
Definitions:
S: Fraction of submodules that are shaded in a given parallel string
X: Fraction of parallel strings in the system that are shaded
m: modules along side of row
n: modules along bottom of row
d:  # of diodes per module
W: module width
L: module length
r: number of rows
Hs: shadow height along inclined plane from Applebaum eqn. A13
g: shadow distance from row edge from Applebaum eq. A12

B: array length along side (m*L in portrait, m*W in landscape configuration) = Appelbaum paper A
beta: effective tilt angle
alpha: solar elevation angle
R: inter-row spacing
phi_bar: average masking angle

*/
	
	double px, py;//, xs, ys;
//	double reduc;

	double S,X;

	//double c1,c2,c3,c3_0;
	//double eqn5, eqn9, eqn10;

	// AppelBaum Appendix A
	double g, Hs;

	if (!solar_transform( solazi, solzen )) return false;

	// Calculate Shading Dimensions
	// Reference Appelbaum and Bany "Shadow effect of adjacent solar collectors in large scale systems" Solar Energy 1979 Vol 23. No. 6
	// if no effective tilt then no array self-shading
	if ( ( (m_zenith_eff < 90.0) && (fabs(m_azimuth_eff) < 90.0) ) && ( m_tilt_eff != 0 ) )
	{ 
		// Appelbaum eqn (12)
		py = m_A * (cosd(m_tilt_eff) + ( cosd(m_azimuth_eff) * sind(m_tilt_eff) /tand(90.0-m_zenith_eff) ) );
		// Appelbaum eqn (11)
		px = m_A * sind(m_tilt_eff) * sind(m_azimuth_eff) / tand(90.0-m_zenith_eff);
	}
	else //! Otherwise the sun has set
	{
		py = 0;
		px = 0;
	}


	// testing
	m_px=px;
	m_py=py;


	// Appelbaum equation A12  Xe = R*Px/Py
	if (py == 0)
		g = 0;
	else
		g = m_R * px / py;

	// Additional constraints from Chris 4/11/12
	g = fabs(g);
	if ( m_arr.mod_orient == 0 ) // Portrait mode
		g = min( g, m_W*m_n );
	else
		g = min( g, m_L*m_n );

	// if number of modules across bottom > number in string and horizontal wiring then g=0
	// Chris Deline email 4/19/12
	if ( ( m_arr.str_orient == 1 ) && ( m_arr.nstrx > 1 ) ) // Horizontal wiring
	{
		g = 0;
	}

	// Appelbaum equation A13  Hs = EF = A(1 - R/Py)
	if (py == 0)
		Hs = 0;
	else
		Hs = m_A * (1.0 - m_R / py);

	// Additional constraints from Chris 4/11/12
	Hs = max( Hs, 0.0);
	if ( m_arr.mod_orient == 0 ) // Portrait mode
	{
		Hs = min( Hs, m_B );
	}
	else
	{
		Hs = min( Hs, m_B );
	}

	m_Xe = g;
	m_Hs = Hs;


	// X and S from Chris Deline 4/23/12
	if ( m_arr.str_orient == 1 ) // Horizontal wiring
	{
		if ( m_arr.mod_orient == 1 ) // Landscape mode
		{
			if ( Hs <= m_W )
			{ // Situation 1a
				X = ( ceil( Hs / m_W ) / (m_m * m_r) ) * ( m_r - 1.0);
				// updated to more conservative approach - email from Chris 4/10/12
				//S = round( Hs * D / W ) / D - floor( Xe / L ) / N;
				S = ( ceil( Hs * m_d / m_W ) / m_d ) * ( 1.0 - floor( g / m_L ) / m_n);
			}
			else // Hs > m_arr.width
			{  // Situation 1b
				X = ( ceil( Hs / m_W ) / (m_m * m_r) ) * ( m_r - 1.0);
			 	S = 1.0;
			}
		}
		else // Portrait mode
		{  // Situation 2
			X = ( ceil( Hs / m_L ) / (m_m * m_r) ) * ( m_r - 1.0);
			S = 1.0 - ( floor( g * m_d / m_W ) / ( m_d * m_n) );
		}
	}
	else // Vertical wiring
	{  // Situation 3
		if ( m_arr.mod_orient == 0 ) // Portrait mode
		{
			X = 1.0 - ( floor( g / m_W ) / m_n );
			S = ( ceil( Hs / m_L ) / ( m_m * m_r ) ) * (m_r - 1.0);
		}
		else // Landscape
		{   // Situation 4
			X = 1.0 - ( floor( g / m_L ) / m_n );
			// updated to more conservative approach - email from Chris 4/10/12
			//S = ( round( Hs * D / W ) / (D * M * R) ) * (R - 1.0);
			S = ( ceil( Hs * m_d / m_W ) / (m_d * m_m * m_r) ) * (m_r - 1.0);
		}
	}

	m_X = X;
	m_S = S;

/* **********************************

// MOVED TO SEPARATE FUNCTION 6/26/2013 (apd)

	m_Gd = Gd_poa; // total plane-of-array diffuse
	m_Gdh = m_Gd * 2 / ( 1.0 + cosd( m_tilt_eff )); // total diffuse on a horizontal surface
	m_Gb = Gb_nor; // beam irradiance (normal to sun rays)
	m_Gbh = m_Gb * cosd( solzen ); // beam on horizontal surface

	// diffuse loss term only
	m_diffuse_loss_term = m_Gdh * ( 1.0 - pow( cosd( m_mask_angle / 2.0), 2) ) * ( m_r - 1.0) / m_r;
	// reduced diffuse radiation
	m_reduced_diffuse = m_Gd - m_diffuse_loss_term;
	// diffuse derate
	if (Gd_poa == 0)
		m_dc_derate = 1.0;
	else
		m_diffuse_derate = m_reduced_diffuse / m_Gd;

	// reduced reflected irradiance
	double F1 = albedo * pow( sind(m_tilt_eff/2.0), 2);
	double solelv = 90.0 - solzen;
	double Y1 = m_R - m_B * sind( 180.0 - solelv - m_tilt_eff ) / sind(solelv);
	Y1 = max(0.0, Y1); // constraint per Chris 4/23/12
	double F2 = 0.5 * albedo * ( 1.0 + Y1/m_B
		- sqrt( pow(Y1,2)/pow(m_B,2) - 2*Y1/m_B * cosd(180 - m_tilt_eff) + 1.0 ) );
	double F3 = 0.5 * albedo * ( 1.0 + m_R/m_B
		- sqrt( pow(m_R,2)/pow(m_B,2) - 2*m_R/m_B * cosd(180 - m_tilt_eff) + 1.0 ) );
	
	m_F1 = F1;
	m_Y1 = Y1;
	m_F2 = F2;
	m_F3 = F3;


	m_Gr1 = F1 * (m_Gbh + m_Gdh);
	m_Gr2 = F2 * m_Gbh + F3 * m_Gdh;

	m_reduced_reflected = ( (F1 + (m_r-1)*F2)/ m_r ) * m_Gbh
		+ ( (F1 + (m_r-1) * F3)/ m_r ) * m_Gdh;

*****************************************************/
	
	diffuse_reduce( solzen, m_tilt_eff, Gb_nor, Gd_poa, 
		m_B/m_R, m_mask_angle, albedo, m_r,

		// outputs
		m_reduced_diffuse, m_diffuse_derate,
		m_reduced_reflected, m_reflected_derate );

	double inc_total =  ( Gb_poa + m_reduced_diffuse + m_reduced_reflected)/1000;
	double inc_diff = (m_reduced_diffuse + m_reduced_reflected)/1000;
	double diffuse_globhoriz = 0;
	if (inc_total != 0)
		diffuse_globhoriz = inc_diff / inc_total;

	// set self-shading coefficients based on C.Deline et al., "A simplified model of uniform shading in large photovoltaic arrays"
	
/***************************
// MOVED TO SEPARATE FUNCTION 6/26/2013 (apd)

	double Xtemp = min( X , 0.65);  // X is limited to 0.65 for c2 calculation
	
	//c1 = 0.25 * exp(( 7.7 - 6.0 * FF0) * X);
	c1 = (109 * FF0 - 54.3) * exp(-4.5 * X); // new c1 on 1/18/13
	
	//c2 = ( 0.145 - 0.095 * FF0) * exp(( 7.7 - 6.0 * FF0) * X);
	c2 = -6 * pow(Xtemp,2) + 5 * Xtemp + 0.28; // new c2 on 1/18/13

	//don't have c4 anymore
	//c4 = 0.17 * ( diffuse_globhoriz ) * ( diffuse_globhoriz	) - 0.16 * ( diffuse_globhoriz ) - 0.004; 
	

	//c3_0 = c4 * X + ( 0.74 * ( diffuse_globhoriz ) - 0.1 ) * FF0 - 0.65 * ( diffuse_globhoriz ) + 0.06;
	c3_0 = (-0.05 * diffuse_globhoriz - 0.01) * X + (0.85 * FF0 - 0.7) * diffuse_globhoriz - 0.085 * FF0 + 0.05;  //new c3_0 on 1/18/13

	c3 = max ( c3_0, ( diffuse_globhoriz ) - 1.0 );

//	if ( c2 != 0)  //don't need check for div by zero any more.
//	{
		//eqn5 = 1.0 - c1 * ( exp( S/c2 - 1.0 ) - 1.0 / exp(1.0) );
		eqn5 = 1.0 - c1 * pow(S,2) - c2 * S;  // new eqn5 on 1/18/13
//	}
//	else
//	{
//		eqn5 = 0;
//	}

	if ( X != 0)
	{
		eqn9 = (X - S) / X;
	}
	else
	{
		eqn9 = 0;
	}

	eqn10 = c3 * ( S - 1.0 ) + ( diffuse_globhoriz );

	reduc = max( eqn5, eqn9);

	reduc = max( reduc, eqn10 );

	m_C1 = c1;
	m_C2 = c2;
	m_C3 = c3;
	m_C3_0 = c3_0;
	m_eqn14 = reduc;

	reduc = X * reduc + (1.0 - X);
	// check limits
	if (reduc > 1) reduc = 1.0;
	if (reduc < 0) reduc = 0.0;

	m_dc_derate = reduc;
	m_eqn5 = eqn5;
	m_eqn9 = eqn9;
	m_eqn10 = eqn10;

	**************************/

	m_dc_derate = selfshade_dc_derate( X, S, FF0, diffuse_globhoriz );

	return true;
}




bool selfshade_t::solar_transform(double solazi, double solzen)
{

    double Snew[3][3];
    double S[3][3];
    double Rot_xy[3][3];
    double Rot_xyz[3][3];
    double Rx[3][3];
    double Ry[3][3];
    double Rz[3][3];

	for (int i=0;i<3;i++)
		for (int j=0;i<3;i++)
		{
			Snew[i][j]=0.0;
			S[i][j]=0.0;
			Rot_xy[i][j]=0.0;
			Rot_xyz[i][j]=0.0;
			Rx[i][j]=0.0;
			Ry[i][j]=0.0;
			Rz[i][j]=0.0;
		}


/*
	// 2011.12.2 - Convention equator=0 rh rotation
	S[0][0] = sind(solzen)*cosd(solazi);
	S[0][1] = 0;
	S[0][2] = 0;
    S[1][0] = -sind(solzen)*sind(solazi);
	S[1][1] = 0;
	S[1][2] = 0;
    S[2][0] = cosd(solzen);
	S[2][1] = 0;
	S[2][2] = 0;
*/
	// 2012.3.21 - new convention north=0 rh rotation
	S[0][0] = sind(solzen)*cosd(solazi+180);
	S[0][1] = 0;
	S[0][2] = 0;
    S[1][0] = -sind(solzen)*sind(solazi+180);
	S[1][1] = 0;
	S[1][2] = 0;
    S[2][0] = cosd(solzen);
	S[2][1] = 0;
	S[2][2] = 0;


    // Calculate Rotation axis around x axis
    Rx[0][0] = 1;
    Rx[0][1] = 0;
    Rx[0][2] = 0;
    Rx[1][0] = 0;
    Rx[1][1] = cosd(m_arr.slope_ew);
    Rx[1][2] = sind(m_arr.slope_ew);
    Rx[2][0] = 0;
    Rx[2][1] = -sind(m_arr.slope_ew);
    Rx[2][2] = cosd(m_arr.slope_ew);

    // Calculate Rotation axis around y axis
    Ry[0][0] = cosd(m_arr.slope_ns);
    Ry[0][1] = 0;
    Ry[0][2] = -sind(m_arr.slope_ns);
    Ry[1][0] = 0;
    Ry[1][1] = 1;
    Ry[1][2] = 0;
    Ry[2][0] = sind(m_arr.slope_ns);
    Ry[2][1] = 0;
    Ry[2][2] = cosd(m_arr.slope_ns);

    // Calculate Rotation axis around z axis
	// assuming that convention correct for 2011.12.2 input, add 180 to degree input for new convention on N=0 instead of equator=0
	// verify results in new system for both northern and southern hemispheres.
	// Appelbaum paper takes 0 to be toward the sum in northern hemisphere - so azimuth south=0
/*	// 2011.12.2 - Convention equator=0 rh rotation
    Rz[0][0] = cosd(m_arr.azimuth);
    Rz[0][1] = -sind(m_arr.azimuth);
    Rz[0][2] = 0;
    Rz[1][0] = sind(m_arr.azimuth);
    Rz[1][1] = cosd(m_arr.azimuth);
    Rz[1][2] = 0;
    Rz[2][0] = 0;
    Rz[2][1] = 0;
    Rz[2][2] = 1;
	*/
	// 2012.3.21 - new convention north=0 rh rotation
    Rz[0][0] = cosd(m_arr.azimuth+180);
    Rz[0][1] = -sind(m_arr.azimuth+180);
    Rz[0][2] = 0;
    Rz[1][0] = sind(m_arr.azimuth+180);
    Rz[1][1] = cosd(m_arr.azimuth+180);
    Rz[1][2] = 0;
    Rz[2][0] = 0;
    Rz[2][1] = 0;
    Rz[2][2] = 1;



    // Calculate complete rotation matrix
    // Find new sun coordinates in transformed euclidian space
	if (!matrix_multiply(Rx,Ry,Rot_xy)) return false;
	if (!matrix_multiply(Rot_xy,Rz,Rot_xyz)) return false;
    if (!matrix_multiply(Rot_xyz,S,Snew)) return false;



	if ((Snew[0][0] < 0) && (Snew[1][0] > 0))
	{
		m_azimuth_eff = atand(-Snew[1][0]/Snew[0][0]) - 180;
	}
    else if ((Snew[0][0] < 0) && (Snew[1][0] < 0))
	{
		m_azimuth_eff = atand(-Snew[1][0]/Snew[0][0]) + 180;
	}
    else if (Snew[0][0] == 0)
	{
        m_azimuth_eff = 90;
	}
    else
	{
        m_azimuth_eff = atand(-Snew[1][0]/Snew[0][0]);
	}

    // Correct for domain of Atand
    if (fabs(Snew[2][0]) < 1e-3)
	{
		m_zenith_eff = 90;
	}
    else if (Snew[2][0] < 0)
	{
		m_zenith_eff = atand(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]) + 180;
	}
    else
	{
        m_zenith_eff = atand(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]);
	}

    return true;

}

bool selfshade_t::matrix_multiply(double a[][3], double b[][3], double c[][3])
{
	// multiplies two 3x3 matrices and and b and places results in matrix c
	int i,j,k;
	for (i=0; i<3; i++)
	{
		for (j=0; j<3; j++)
		{
			double sum = 0;
			for (k=0; k<3; k++)
			{
				sum = sum + a[i][k]*b[k][j];
			}
			c[i][j] = sum;
		}
	}
	return true;
}

