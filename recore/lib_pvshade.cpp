#include "lib_pvshade.h"
#include <math.h>
#include <limits>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif


selfshade_t::selfshade_t()
{
}


selfshade_t::selfshade_t( ssarrdat &arr,
		double solzen,
		double solazi,
		double beamnorm,
		double globhoriz )
{
// Geometry calculations ported from sam_shading_type241.f90
// Find Effective Angles (i.e. transform sun's position with respect to tilted ground
//	solar_transform(azimuth,zenith,s_azimuth,azimuth_eff,zenith_eff,slope_ew,slope_ns);
//	tilt_eff = tilt - slope_ns;

/*	! Calculate Shading Dimensions
	! Reference Appelbaum and Bany "Shadow effect of adjacent solar collectors in large scale systems" Solar Energy 1979 Vol 23. No. 6
	if (ZENITH_EFF .LT. 90 .AND. ABS(AZIMUTH_EFF) .LT. 90) then
		PY = LROWS*(COSD(TILT_EFF)+(COSD(AZIMUTH_EFF)*SIND(TILT_EFF)/TAND(90-ZENITH_EFF)))
		PX = LROWS*SIND(TILT_EFF)*SIND(AZIMUTH_EFF)/TAND(90-ZENITH_EFF)
		XS = MAX(0.,WROWS-ABS(ROW_SPACE*(PX/PY)))
		YS = MAX(0.,LROWS*(1-ROW_SPACE/PY))
	else                                ! Otherwise the sun has set
		PY = 0.
		PX = 0.
		XS = 0.
		YS = 0.                     
	endif
*/
	solar_transform(solazi,solzen,arr);
	// TODO - test example in Appelbaum's paper
	// Adjust for PVWatts convention
}

bool selfshade_t::solar_transform(double &solazi, double &solzen, ssarrdat &arr)
{

//    double azimuth,zenith,s_azimuth,azimuth_eff,zenith_eff,slope_ew,slope_ns;
    double Snew[3][3];
    double S[3][3];
    double Rot[3][3];
    double Rx[3][3];
    double Ry[3][3];
    double Rz[3][3];
    
    // Convert sun coordinates into Euclidian space
	S[0][0] = sin(solzen)*cos(solazi);
    S[1][0] = -sin(solzen)*sin(solazi);
    S[2][0] = cos(solzen);

    // Calculate Rotation axis around x axis
    Rx[0][0] = 1;
    Rx[0][1] = 0;
    Rx[0][2] = 0;
    Rx[1][0] = 0;
    Rx[1][1] = cos(arr.slope_ew);
    Rx[1][2] = sin(arr.slope_ew);
    Rx[2][0] = 0;
    Rx[2][1] = -sin(arr.slope_ew);
    Rx[2][2] = cos(arr.slope_ew);

    // Calculate Rotation axis around y axis
    Ry[0][0] = cos(arr.slope_ns);
    Ry[0][1] = 0;
    Ry[0][2] = -sin(arr.slope_ns);
    Ry[1][0] = 0;
    Ry[1][1] = 1;
    Ry[1][2] = 0;
    Ry[2][0] = sin(arr.slope_ns);
    Ry[2][1] = 0;
    Ry[2][2] = cos(arr.slope_ns);

    // Calculate Rotation axis around z axis
    Rz[0][0] = cos(arr.azimuth);
    Rz[0][1] = -sin(arr.azimuth);
    Rz[0][2] = 0;
    Rz[1][0] = sin(arr.azimuth);
    Rz[1][1] = cos(arr.azimuth);
    Rz[1][2] = 0;
    Rz[2][0] = 0;
    Rz[2][1] = 0;
    Rz[2][2] = 1;

    // Calculate complete rotation matrix
	if (!matrix_multiply(Rx,Ry,Rot)) return false;
	if (!matrix_multiply(Rot,Rz,Rot)) return false;

    // Find new sun coordinates in transformed euclidian space
    if (!matrix_multiply(Rot,S,Snew)) return false;

    // Calculate effective angles
    // Correct for domain of Atand
    if ((Snew[0][0] < 0) && (Snew[1][0] > 0)) 
	{
		arr.azimuth = atan(-Snew[1][0]/Snew[0][0]) - 180;
	}
    else if ((Snew[0][0] < 0) && (Snew[1][0] < 0))
	{
		arr.azimuth = atan(-Snew[1][0]/Snew[0][0]) + 180;
	}
    else if (Snew[0][0] == 0) 
	{
        arr.azimuth = 90;
	}
    else
	{
        arr.azimuth = atan(-Snew[1][0]/Snew[0][0]);
	}

    // Correct for domain of Atand
    if (Snew[2][0] == 0)
	{
		arr.tilt = 90;
	}
    else if (Snew[2][0] < 0)
	{
		arr.tilt = atan(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]) + 180;
	}
    else
	{
        arr.tilt = atan(sqrt(Snew[0][0]*Snew[0][0]+Snew[1][0]*Snew[1][0])/Snew[2][0]);
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