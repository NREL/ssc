#include "lib_pvshade.h"
#include <math.h>
#include <limits>
#include <sstream>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

double min( double a, double b )
{
	return (a < b) ? a : b;
}

double max( double a, double b )
{
	return (a > b) ? a : b;
}

double cosd( double degrees )
{
	return cos( degrees * M_PI / 180.0 );
}

double sind( double degrees )
{
	return sin( degrees * M_PI / 180.0 );
}

double tand( double degrees )
{
	return tan( degrees * M_PI / 180.0 );
}

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


selfshade_t::selfshade_t()
{
}


selfshade_t::selfshade_t( ssarrdat &arr )
{
	m_arr = arr;
}



bool selfshade_t::exec(
		double solzen,
		double solazi,
		double inc_total,
		double inc_diff,
		double FF0)
{
/*
Chris Deline 4/9/2012 

Definitions of X and S in SAM for the four layout conditions – portrait, landscape and vertical / horizontal strings.
Definitions:
S: Fraction of submodules that are shaded in a given parallel string
X: Fraction of parallel strings in the system that are shaded
M: modules along side of row
N: modules along bottom of row
D:  # of diodes per module
W: module width
L: module length
R: number of rows
Hs: shadow height along inclined plane from Applebaum eqn. A13
Xe: shadow distance from row edge from Applebaum eq. A12

Situation 1a: Horizontal string, landscape orientation.  Hs ≤ W
X=(R-1)/MR
S=(round ((H_s D)⁄W))/D-floor(X_e⁄L)/N
Situation 1b: Horizontal string, landscape orientation.  Hs>W  (*not an exact calculation) 
X=ceil(H_s⁄W)/MR(R-1)
S=1

Situation 2: Horizontal string, portrait orientation.
X=ceil(H_s⁄L)/MR(R-1)
S=1-floor((X_e D)⁄W)/DN

Situation 3: Vertical string, portrait orientation.
X=1-(floor (X_e/W))⁄N
S=ceil(H_s⁄L)/MR(R-1)

Situation 4: Vertical string, landscape orientation.
X=1-floor(X_e/L)⁄N
S=(round ((H_s D)⁄W))/DMR(R-1)

*/
	double px, py;//, xs, ys;
	double tilt_eff;
	double reduc;

	//Cdeline_simplified model of uniform shading _v1.docx
	double S,X; 
	double c1,c2,c3,c3_0,c4;
	double eqn5, eqn9, eqn10;

	// AppelBaum Appendix A
	double Xe, Hs, A;


	double diffuse_globhoriz = 0;
	// handle globalhoriz == 0
	if (inc_total != 0)
	{
		diffuse_globhoriz = inc_diff / inc_total;
	}

	//! Find Effective Angles (i.e. transform sun's position with respect to tilted ground

	if (m_arr.mod_orient == 0) //            ! Portrait Mode
		A = m_arr.length * m_arr.nmody;
	else //                                   ! Landscape Mode
		A = m_arr.width * m_arr.nmody;




	if (!solar_transform( solazi, solzen )) return false;
	tilt_eff = m_arr.tilt - m_arr.slope_ns;
	

	// Calculate Shading Dimensions
	// Reference Appelbaum and Bany "Shadow effect of adjacent solar collectors in large scale systems" Solar Energy 1979 Vol 23. No. 6
	// if no effective tilt then no array self-shading
	if ( ( (zenith_eff < 90.0) && (abs(azimuth_eff) < 90.0) ) && ( tilt_eff != 0 ) )
	{
		// Appelbaum eqn (12)
		py = A * (cosd(tilt_eff) + ( cosd(azimuth_eff) * sind(tilt_eff) /tand(90.0-zenith_eff) ) );
		// Appelbaum eqn (11)
		px = A * sind(tilt_eff) * sind(azimuth_eff) / tand(90.0-zenith_eff);
	}
	else //! Otherwise the sun has set
	{
		py = 0;
		px = 0;
	}


	// testing
	m_px=px;
	m_py=py;
	m_azi_eff=azimuth_eff;
	m_zen_eff=zenith_eff;


	double M = m_arr.nmody; 
	double N = m_arr.nmodx; 
	double D = m_arr.ndiode;
	double W = m_arr.width;
	double L = m_arr.length;
	double R = m_arr.nrows;

	// Appelbaum equation A12  Xe = R*Px/Py
	if (py == 0)
		Xe = 0;
	else
		Xe = m_arr.row_space * px / py;

	// Additional constraints from Chris 4/11/12
	Xe = abs(Xe);
	if ( m_arr.mod_orient == 0 ) // Portrait mode
		Xe = min( Xe, W*N );
	else
		Xe = min( Xe, L*N );

	// Appelbaum equation A13  Hs = EF = A(1 - R/Py)
	if (py == 0)
		Hs = 0;
	else
		Hs = A * (1.0 - m_arr.row_space / py);

	// Additional constraints from Chris 4/11/12
	Hs = max( Hs, 0.0);
	if ( m_arr.mod_orient == 0 ) // Portrait mode
		Hs = min( Hs, L*M );
	else
		Hs = min( Hs, W*M );


	m_Xe = Xe;
	m_Hs = Hs;

	// X and S from Chris Deline 4/9/12 
	if ( m_arr.str_orient == 1 ) // Horizontal wiring
	{
		if ( m_arr.mod_orient == 1 ) // Landscape mode
		{
			if ( Hs <= W )
			{ // Situation 1a
				X = (R - 1.0) / ( M * R);
				// updated to more conservative approach - email from Chris 4/10/12
				//S = round( Hs * D / W ) / D - floor( Xe / L ) / N;
				S = ceil( Hs * D / W ) / D - floor( Xe / L ) / N;
			}
			else // Hs > m_arr.width
			{  // Situation 1b
				X = ( ceil( Hs / W ) / (M * R) ) * ( R - 1.0);
			 	S = 1.0;
			}
		}
		else // Portrait mode
		{  // Situation 2
			X = ( ceil( Hs / L ) / (M * R) ) * ( R - 1.0);
			S = 1.0 - ( floor( Xe * D / W ) / ( D * N) );
		}
	}
	else // Vertical wiring
	{  // Situation 3
		if ( m_arr.mod_orient == 0 ) // Portrait mode
		{
			X = 1.0 - ( floor( Xe / W ) / N );
			S = ( ceil( Hs / L ) / ( M * R ) ) * (R - 1.0);
		}
		else // Landscape
		{   // Situation 4
			X = 1.0 - ( floor( Xe / L ) / N );
			// updated to more conservative approach - email from Chris 4/10/12
			//S = ( round( Hs * D / W ) / (D * M * R) ) * (R - 1.0);
			S = ( ceil( Hs * D / W ) / (D * M * R) ) * (R - 1.0);
		}
	}


	m_X = X;
	m_S = S;


	c1 = 0.25 * exp( 7.7 - 6.0 * FF0) * X;

	c2 = ( 0.145 - 0.095 * FF0) * exp( 7.7 - 6.0 * FF0) * X;

	c4 = 0.17 * ( diffuse_globhoriz ) * ( diffuse_globhoriz	) - 0.16 * ( diffuse_globhoriz ) - 0.004;

	c3_0 = c4 * X + ( 0.74 * ( diffuse_globhoriz ) - 0.1 ) * FF0 - 0.65 * ( diffuse_globhoriz ) + 0.06;

	c3 = max ( c3_0, ( diffuse_globhoriz ) - 1.0 );

	if ( c2 != 0)
	{
		eqn5 = 1.0 - c1 * ( exp( S/c2 - 1.0 ) - 1.0 / exp(1.0) );
	}
	else
	{
		eqn5 = 0;
	}

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
	m_C4 = c4;
	m_eqn14 = reduc;

	reduc = X * reduc + (1.0 - X);
	// check limits
	if (reduc > 1) reduc = 1.0;
	if (reduc < 0) reduc = 0.0;


	m_dc_derate = reduc;
	m_eqn5 = eqn5;
	m_eqn9 = eqn9;
	m_eqn10 = eqn10;

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


    // Convert sun coordinates into Euclidian space
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
		azimuth_eff = atand(-Snew[1][0]/Snew[0][0]) - 180;
	}
    else if ((Snew[0][0] < 0) && (Snew[1][0] < 0))
	{
		azimuth_eff = atand(-Snew[1][0]/Snew[0][0]) + 180;
	}
    else if (Snew[0][0] == 0) 
	{
        azimuth_eff = 90;
	}
    else
	{
        azimuth_eff = atand(-Snew[1][0]/Snew[0][0]);
	}

    // Correct for domain of Atand
//    if (Snew[2][0] == 0)
    if (abs(Snew[2][0]) < 1e-3)
	{
		zenith_eff = 90;
	}
    else if (Snew[2][0] < 0)
	{
		zenith_eff = atand(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]) + 180;
	}
    else
	{
        zenith_eff = atand(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]);
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