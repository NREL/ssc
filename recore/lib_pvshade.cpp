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
		double beamnorm,
		double globhoriz,
		double diffuse,
		double FF0)
{
// Geometry calculations ported from sam_shading_type241.f90
/*

! Parameters
!--------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Units  
!---|--------------------------------|-----------------------------------------------------------------|-------------
!  1| ENABLE                         | 1 enable shading calculations 0 to disable                      | none
!  2| NMODX                          | Number of modules in row in x direction                         | none
!  3| NMODY                          | Number of modules in row in y direction                         | none
!  4| NROWS                          | Number of rows                                                  | none
!  5| LENGTH                         | Length of module                                                | meters
!  6| WIDTH                          | Width of module                                                 | meters
!  7| MOD_ORIENT                     | Oreintation of Panel (0 if Portrait 1 if Landscape)             | none
!  8| STR_ORIENT                     | Oreintation of Strings (0 if Vertical 1 if Horizontal)          | none
!  9| ROW_SPACE                      | Spacing between rows                                            | meters
! 10| MOD_SPACE                      | Spacing between modules in row                                  | meters
! 11| NCELX                          | Number of cells per module in x direction                       | none
! 12| NCELY                          | Number of cells per module in y direction                       | none
! 13| NDIODE                         | Number of bypass diodes per module                              | none
! 14| SLOPE_NS                       | Slope of Terrain in N/S direction                               | degrees
! 15| SLOPE_EW                       | Slope of Terrain in E/W direction                               | degrees
! 16| PV_MODE                        | PV Model used (2 = sandia, 1 = CEC model)                       | none
! 17| Vmp                            | Maximum Power point voltage                                     | Volts
! 18| Imp                            | Maximum Power point current                                     | Amps
! 19| Voc                            | Open Circuit Voltage                                            | Volts
! 20| Ixx,I_L                        | Ixx for sandia,I_L for CEC                                      | Amps
! 21| Isc,I_0                        | Isc for sandia,I_0 for CEC                                      | Amps

! Inputs
!---------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Units  
!---|--------------------------------|-----------------------------------------------------------------|--------------
!  1| ZENITH                         | Solar Zenith Angle                                              | degrees
!  2| S_AZIMUTH                      | Solar Azimuth Angle                                             | degrees  
!  3| TILT                           | Tilt of Collectors                                              | degrees    
!  4| AZIMUTH                        | Surface Azimth                                                  | degrees
!  5| B                              | Beam irradiance on surface                                      | kJ/hr
!  6| G                              | Global irradiance on surface                                    | kJ/hr
!  7| V                              | Operating Voltage of Module                                     | Volts
!  8| P0                             | Module output before derate                                     | Watts
 
! Outputs
!----------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Units  
!---|--------------------------------|-----------------------------------------------------------------|---------------
!  1| REDUC                          |  Array Reduction Factor (non-linear)                            | none      
!  2| SHADE_AREA                     |  Fraction of aray area that is shaded (linear)                  | none
!3-5| C                              |  Least squares regression coefficients                          | none
!  6| shad_error                     |  -1 = rmse error, 0 = no error, 1 = too much shading            | none


! Local variables
!! Axis Rotation/Effective Angles
real(8) ZENITH_EFF     ! Effective zenith angle
real(8) AZIMUTH_EFF    ! Effective azimuth angle
real(8) TILT_EFF       ! Effective tilt angle

!! Landscape/Portrait Rotation (Orientation)
real(8) W              ! Width, correcting for orientation
real(8) L              ! Length, correcting for orientation
integer NCSUBX         ! Number of cells in x direction, correcting for orientation
integer NCSUBY         ! Number of cells in y direction, correcting for orientation

!! Calculate Components of shade
real(8) PX             ! X component of shadow on ground
real(8) PY             ! Y component of shadow on ground
real(8) XS             ! distance along x axis from edge of row to beginning of shaded area
real(8) YS             ! distance along y axis from edge of row to beginning of shaded area

!! Calculate Reuduction factor based on cell geometry
real(8) WROWS          ! Length of rows (meters)
real(8) LROWS          ! Width of Rows (meters)
real(8) NSUBX          ! Number of Substrings in x direction
real(8) NSUBY          ! Number of substrings in y direction
real(8) LSUBX          ! Length of substrings in x direction
real(8) LSUBY          ! Length of substrings in y direction
integer NSTR_S         ! Number of shaded strings in y direction
integer NSTR_FS        ! Number of fully shaded strings in y direction
integer NSTR_PS        ! Number of partially shaded strings in y direction
integer NSTR_US        ! Number of unshaded strings
integer NSTR           ! Number of strings
real(8) FSUBX_FS       ! Fraction of substrings fully shaded in x direction
real(8) FSUBY_FS       ! Fraction of substrings fully shaded in y direction
real(8) FSUBX_PS       ! Fraction of substrings partially shaded in x direction
real(8) FSUBY_PS       ! Fraction of substrings partially shaded in y direction
real(8) FSUB_FS        ! Fraction of substrings fully shaded
real(8) FSUB_PS        ! Fraction of substrings partially shaded
integer I              ! Index variable
real(8) FS_SPR         ! Shaded percentage ratio for fully shaded row
real(8) PS_SPR         ! Shaded percentage ratio for partially shaded row
real(8) DT_PS           ! Diode turnon for partially shaded row
real(8) DT_FS           ! Diode turnon for fully shaded row
real(8) F_XS
real(8) F_YS
real(8) test1,test2

! Outputs
real(8) REDUC, SHADE_AREA, c(3)
integer shad_error

*/
	double px, py, xs, ys;
	double tilt_eff;
	double l,w;
	int ncsubx, ncsuby;
	bool finished = false;
	double small = 1e-4;
	double reduc = 0, shade_area = 0;
	int shad_error = 0;
	double wrows;          // Length of rows (meters)
	double lrows;          // Width of Rows (meters)
	double nsubx;          // Number of Substrings in x direction
	double nsuby;          // Number of substrings in y direction
	double lsubx;          // Length of substrings in x direction
	double lsuby;          // Length of substrings in y direction
	int nstr_s;         // Number of shaded strings in y direction
	int nstr_fs;        // Number of fully shaded strings in y direction
	int nstr_ps;        // Number of partially shaded strings in y direction
	int nstr_us;        // Number of unshaded strings
	int nstr;           // Number of strings
	double fsubx_fs;       // Fraction of substrings fully shaded in x direction
	double fsuby_fs;       // Fraction of substrings fully shaded in y direction
	double fsubx_ps;       // Fraction of substrings partially shaded in x direction
	double fsuby_ps;       // Fraction of substrings partially shaded in y direction
	double fsub_fs;        // Fraction of substrings fully shaded
	double fsub_ps;        // Fraction of substrings partially shaded
	//int i;              // Index variable
	double fs_spr=0;         // Shaded percentage ratio for fully shaded row
	double ps_spr=0;         // Shaded percentage ratio for partially shaded row
	//double dt_ps;           // Diode turnon for partially shaded row
	//double dt_fs;           // Diode turnon for fully shaded row
	double f_xs;
	double f_ys;


	//Cdeline_simplified model of uniform shading _v1.docx
	double S,X; 
	double c1,c2,c3,c3_0,c4;
	double eqn5, eqn9, eqn10;


	// Determine panel orientation, and flip dimensions if landscape
	if (m_arr.mod_orient == 0) //            ! Portrait Mode
	{
		l = m_arr.length;
		w = m_arr.width;
		ncsubx = m_arr.ncellx / m_arr.ndiode;
		ncsuby = m_arr.ncelly;
	}
	else //                                   ! Landscape Mode
	{
		l = m_arr.width;
		w = m_arr.length;
		ncsubx = m_arr.ncelly;
		ncsuby = m_arr.ncellx / m_arr.ndiode;
	}

	// Calculate System Dimensions
	wrows = m_arr.nmodx * w;
	lrows = m_arr.nmody * l;

	//! Find Effective Angles (i.e. transform sun's position with respect to tilted ground
//call SolarTransform(azimuth,zenith,s_azimuth,azimuth_eff,zenith_eff,slope_ew,slope_ns)

	if (!solar_transform( solazi, solzen )) return false;
	tilt_eff = m_arr.tilt - m_arr.slope_ns;
	
	// Calculate Shading Dimensions
	// Reference Appelbaum and Bany "Shadow effect of adjacent solar collectors in large scale systems" Solar Energy 1979 Vol 23. No. 6
	if ( (zenith_eff < 90.0) && (abs(azimuth_eff) < 90.0) ) 
	{
		py = lrows * (cosd(tilt_eff) + ( cosd(azimuth_eff) * sind(tilt_eff) /tand(90.0-zenith_eff) ) );
		px = lrows * sind(tilt_eff) * sind(azimuth_eff) / tand(90.0-zenith_eff);
		xs = max( 0., wrows - abs(m_arr.row_space*(px/py) ) );
		ys = max( 0., lrows * (1.0 - m_arr.row_space/py) );
	}
	else //! Otherwise the sun has set
	{
		py = 0;
		px = 0;
		xs = 0;
		ys = 0;                     
	}

	// testing
	m_xs=xs;
	m_ys=ys;
	m_px=px;
	m_py=py;
	m_lrows=lrows;
	m_wrows=wrows;
	m_azi_eff=azimuth_eff;
	m_zen_eff=zenith_eff;

	if ( (ys <= 0.0) && (xs <= 0.0) )
	{
		shade_area = 1.0;
		reduc = 1.0;
	}
	else
	{
		shade_area = 1.0-( ( (m_arr.nrows-1.0) * ys * xs ) / (wrows * lrows * m_arr.nrows) );

		if (m_arr.str_orient == 1) // Horiztonal Strings
		{
		// Find number of strings, and number in each shading class (shaded,unshaded,fully shaded, partially shaded)
			nstr = m_arr.nmody;
			nstr_s = (int)ceil(ys/1.0);
			nstr_fs = int(ys/1.0);
			nstr_ps = nstr_s - nstr_fs;
			nstr_us = nstr - nstr_s;
    
			// Number and length of substrings in x,y directions
			if (m_arr.mod_orient == 0) 
			{
				nsubx = m_arr.nmodx*m_arr.ndiode;
				nsuby = 1;
				lsubx = w/m_arr.ndiode;
				lsuby = l; 
			}
			else
			{
				nsubx = m_arr.nmodx;
				nsuby = m_arr.ndiode;
				lsubx = w;
				lsuby = l/m_arr.ndiode;
			}

			// Find Fraction of substrings shaded in x,y directions
			fsubx_fs = int(xs/lsubx)/nsubx;
			fsuby_fs = 1.;
			fsub_fs = fsubx_fs*fsuby_fs;
			fsubx_ps = int(xs/lsubx)/nsubx;
			fsuby_ps = int((ys-nstr_fs*l)/lsuby)/nsuby;
			fsub_ps = fsubx_ps*fsuby_ps;
    
			/*
			! Find diode turnon threshold
			! Removed becuase we have no good measurements of diode turn-on. Instead we hardwire the threshold to 0.25.
			!call diodeturnon(B,G,FSUB_PS,DT_PS)
			!call diodeturnon(B,G,FSUB_FS,DT_FS)
			*/
			// Find percent of substrings shaded
			f_xs = (xs - int(xs/lsubx)*lsubx)/lsubx;
			f_ys = ((ys-nstr_fs*l) - int((ys-nstr_fs*l)/lsuby)*lsuby)/lsuby;
    
			// Determine whether threshold is reached, and turn on or do nothing
			//if (F_XS .GE. DT_FS/NCSUBX) then
			if ( (f_xs*beamnorm/globhoriz) >= (.25/ncsubx) )
			{
				fsubx_fs = ceil(xs/lsubx)/nsubx;
				fsubx_ps = ceil(xs/lsubx)/nsubx;
				fsuby_fs = 1.;
			}
			else
			{
				fsubx_fs = int(xs/lsubx)/nsubx;
				fsubx_ps = int(xs/lsubx)/nsubx;
				fsuby_fs = 1.;
			}

			//! Do the same in y direction
			//!if (F_YS .GE. DT_PS/NCSUBY) then                   <- expression before hardwiring diode turnon threshold.
			if ( (f_ys*beamnorm/globhoriz) >= (.25/ncsuby) )
			{
				fsuby_ps = ceil((ys-nstr_fs*l)/lsuby)/nsuby;
			}
			else
			{
				fsuby_ps = int((ys-nstr_fs*l)/lsuby)/nsuby;
			}
    
			// Find fraction of substrings shaded (FS and PS)
			fsub_fs = fsubx_fs*fsuby_fs;
			fsub_ps = fsubx_ps*fsuby_ps;

		/*
			// From:Some notes on partial shading implementation in SAM by Chris Deline (emailed Fall 2010)
			// Find operating voltage for shaded substrings
			if (abs(1-FSUB_FS) < small .or. FSUB_FS > 1 .or. FSUB_FS < 0-small) then
				Vop(1) = Voc+1              !flagging bad value later set to 0 power
			else
				Vop(1) = Vmp/(1-FSUB_FS)
			end if
    
			if (1-FSUB_PS < small .or. FSUB_PS > 1 .or. FSUB_PS < 0-small) then
				Vop(2) = Voc+1
			else
				Vop(2) = Vmp/(1-FSUB_PS)
			end if
    
			! Find operating current using exponential fit
			Iop(1) = c(1)-c(2)*exp(c(3)*Vop(1))
			Iop(2) = c(1)-c(2)*exp(c(3)*Vop(2))
    
			! If there is no shading, make sure power is at Pmp (corrects for bad exponential fits)
			do i =1,2 
				if (Vop(i) .eq. Vmp) then
					Vop(i) = Vmp 
					Iop(i) = Imp
				end if
			end do
    
			! Make sure Vop is not greater than Voc. 
			! if Vop > Voc push power to 0
			! otherwise find derated power
			if (Vop(1) > Voc) then
				FS_SPR = 0.
			else
				FS_SPR = (1-FSUB_FS)*Vop(1)*Iop(1)/Pmp
			end if
        
			if (Vop(2) > Voc) then
				PS_SPR = 0.
			else
				PS_SPR = (1-FSUB_PS)*Vop(2)*Iop(2)/Pmp
			end if
    
			! Find total array derate
			REDUC = (1.+(NROWS-1)*(NSTR_PS*PS_SPR+NSTR_FS*FS_SPR+NSTR_US*1.)/NSTR)/NROWS
		  */  
		}
		else   // Vertical Strings
		{
			nstr = m_arr.nmodx;
			nstr_s = (int)ceil(xs/w);
			nstr_fs = int(xs/w);
			nstr_ps = nstr_s - nstr_fs;
			nstr_us = nstr - nstr_s;
    
			if (m_arr.mod_orient == 0) 
			{
				nsubx = m_arr.ndiode;
				nsuby = m_arr.nmody;
				lsubx = w/m_arr.ndiode;
				lsuby = l;
			}
			else
			{
				nsubx = 1;
				nsuby = m_arr.ndiode * m_arr.nmody;
				lsubx = w;
				lsuby = l/ m_arr.ndiode;
			}

			fsubx_fs = 1.;
			fsuby_fs = int(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
			fsubx_ps = int((xs-nstr_fs*w)/lsubx)/nsubx;
			fsuby_ps = int(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
                
			//call diodeturnon(B,G,FSUB_PS,DT_PS)
			//call diodeturnon(B,G,FSUB_FS,DT_FS)
    
			f_xs = ( (xs-nstr_fs*w) - int( (xs-nstr_fs*w) / lsubx ) * lsubx ) / lsubx;
			f_ys = (ys - int(ys/lsuby) *lsuby ) / lsuby;
    
			//!if (F_YS .GE. DT_FS/NCSUBY) then
			if ( (f_ys*beamnorm/globhoriz) >= (.25/ncsuby) )
			{
				fsubx_fs = 1.;
				fsuby_fs = ceil(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
				fsuby_ps = ceil(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
			}
			else
			{
				fsubx_fs = 1.;
				fsuby_fs = int(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
				fsuby_ps = int(ys/lsuby)/nsuby*(m_arr.nrows-1)/m_arr.nrows;
			}

			if ( (f_xs*beamnorm/globhoriz) >= (.25/ncsuby) )
			{
				fsubx_ps = ceil((xs-nstr_fs*w)/lsubx)/nsubx;
			}
			else
			{
				fsubx_ps = int((xs-nstr_fs*w)/lsubx)/nsubx;
			}
    
			fsub_ps = (fsubx_ps*fsuby_ps)*(m_arr.nrows-1)/m_arr.nrows;
			fsub_fs = (fsubx_fs*fsuby_fs)*(m_arr.nrows-1)/m_arr.nrows;
    
	/*
			! Find operating voltage for shaded substrings
			if (abs(1-FSUB_FS) < small .or. FSUB_FS > 1 .or. FSUB_FS < 0-small) then
				Vop(1) = Voc+1
			else
				Vop(1) = Vmp/(1-FSUB_FS)
			end if
    
			if (1-FSUB_PS < small .or. FSUB_PS > 1 .or. FSUB_PS < 0-small) then
				Vop(2) = Voc+1
			else
				Vop(2) = Vmp/(1-FSUB_PS)
			end if
    
			! Find operating current using exponential fit
			Iop(1) = c(1)-c(2)*exp(c(3)*Vop(1))
			Iop(2) = c(1)-c(2)*exp(c(3)*Vop(2))
    
			! If there is no shading, make sure power is at Pmp (corrects for bad exponential fits)
			do i =1,2 
				if (Vop(i) .eq. Vmp) then
					Vop(i) = Vmp 
					Iop(i) = Imp
				end if
			end do
    
			! Make sure Vop is not greater than Voc. 
			! if Vop > Voc push power to 0
			! otherwise find derated power
			if (Vop(1) > Voc) then
				FS_SPR = 0.
			else
				FS_SPR = (1-FSUB_FS)*Vop(1)*Iop(1)/Pmp
			end if
        
			if (Vop(2) > Voc) then
				PS_SPR = 0.
			else
				PS_SPR = (1-FSUB_PS)*Vop(2)*Iop(2)/Pmp
			end if
    
			REDUC = (NSTR_PS*PS_SPR+NSTR_FS*FS_SPR+NSTR_US*1.)/NSTR
	*/
		}  // string orientation

	
	// Update from Chris Deline "Cdeline_simplified model of uniform shading _v1.docx" 3/8/12
		//FF0 = pmp / voc / isc;

		//  - assumption is that partially shaded is more that half cells in substrings
		X = (nstr_fs + nstr_ps) / nstr;  // shaded parallel strings
		S = fsub_fs + fsub_ps;  // shaded sub modules

		c1 = 0.25 * exp( 7.7 - 6.0 * FF0) * X;

		c2 = ( 0.145 - 0.095 * FF0) * exp( 7.7 - 6.0 * FF0) * X;

		c4 = 0.17 * ( diffuse/globhoriz ) * ( diffuse/globhoriz ) - 0.16 * ( diffuse/globhoriz ) - 0.004;

		c3_0 = c4 * X + ( 0.74 * ( diffuse/globhoriz ) - 0.1 ) * FF0 - 0.65 * ( diffuse/globhoriz ) + 0.06;

		c3 = max ( c3_0, ( diffuse/globhoriz ) - 1.0 );

		if ( c2 != 0)
		{
			eqn5 = 1.0 - c1 * ( exp( S/c2 - 1.0 ) - 1.0 / exp(1.0) );
		}
		else
		{
			eqn5 = -DBL_MAX;
		}

		if ( X != 0)
		{
			eqn9 = (X - S) / X;
		}
		else
		{
			eqn9 = -DBL_MAX;
		}

		eqn10 = c3 * ( S - 1.0 ) + ( diffuse/globhoriz );

		reduc = max( eqn5, eqn9);

		reduc = max( reduc, eqn10 );

		reduc = X * reduc + (1.0 - X);
		// derate
		reduc = 1.0 - reduc;
	}
	if (reduc <= 0) reduc = 1.0;
	m_dc_derate = reduc;
	m_shade_area = shade_area;

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
	S[0][0] = sind(solzen)*cosd(solazi);
	S[0][1] = 0;
	S[0][2] = 0;
    S[1][0] = -sind(solzen)*sind(solazi);
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
	// 2011.12.2 - Convention equator=0 rh rotation
    Rz[0][0] = cosd(m_arr.azimuth);
    Rz[0][1] = -sind(m_arr.azimuth);
    Rz[0][2] = 0;
    Rz[1][0] = sind(m_arr.azimuth);
    Rz[1][1] = cosd(m_arr.azimuth);
    Rz[1][2] = 0;
    Rz[2][0] = 0;
    Rz[2][1] = 0;
    Rz[2][2] = 1;
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