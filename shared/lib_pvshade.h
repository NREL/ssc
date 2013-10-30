#ifndef __pvshade_h
#define __pvshade_h

#include <string>

#include "lib_util.h"


//   Porting of sam_shading_type241.f90 to new orientation

struct ssarrdat
{
	double tilt, azimuth;
	int nstrx, nmodx, nmody, nrows;
	double length, width;
	int mod_orient, str_orient;
	double row_space, mod_space;
	int ncellx, ncelly, ndiode;
	double slope_ns, slope_ew;
	int mask_angle_calc_method;
};

bool selfshade_simple(

	/* system parameters */
	int ncells, // number of cells in panel
	double area, // panel area in m2
	int orientation, // 0 = landscape, 1 = portrait
	int panels_up, // number of panels along the edge of a row
	double FF_stc, // fill factor @ STC = Pmp0 / Voc0 / Isc0;

	/* current conditions */
	double solzen, // solar zenith angle (deg)
	double solazi, // solar azimuth angle (deg)
	double beam_horiz, // beam irradiance on the horizontal surface (W/m2)
	double diff_poa, // total diffuse irradiance on the tilted surface (W/m2)
	double albedo, // ground reflectance [0..1]

	/* calculated outputs */
	double *dc_derate,
	double *skydiff_derate,
	double *gnddiff_derate );



void diffuse_reduce( 
		double solzen,
		double stilt,
		double Gb_nor,
		double Gd_poa,
		double gcr,
		double phi0, // mask angle (degrees)
		double alb,
		double nrows,
		
		double &reduced_skydiff,
		double &Fskydiff,
		double &reduced_gnddiff,
		double &Fgnddiff );



double selfshade_dc_derate( double X, 
						   double S, 
						   double FF0, 
						   double dbh_ratio );


void selfshade_xs_horstr( bool landscape, // modules oriented in landscape/portrait on assembly
						   double W,   // module width (short side)
						   double L,   // module length (long side)
						   int r,      // number of rows
						   int m,      // number of modules along row edge (short side of assembly)
						   int n,      // number of modules along (long side of assembly)
						   int ndiode, // number of bypass diodes
						   double Fshad, // Fraction of assembly shaded up from long edge

						   // outputs
						   double &X, double &S);

class selfshade_t
{
public:
	selfshade_t();
	selfshade_t( ssarrdat &arr );

	bool exec( 
		double solzen,    // solar zenith (deg)
		double solazi,    // solar azimuth (deg)
		double Gb_nor,    // beam normal irradiance (W/m2)
		double Gb_poa,    // POA beam irradiance (W/m2)
		double Gd_poa,    // POA diffuse, sky+gnd (W/m2)
		double FF0,       // Fill Factor at STC = Pmp0 / Voc0 / Isc0;
		double albedo);   // used to calculate reduced relected irradiance

	void set_ssarrdat( ssarrdat &arr ) {m_arr = arr;}
	double dc_derate() {return m_dc_derate;}
	double reduced_diffuse() {return m_reduced_diffuse;}
	double reduced_reflected() {return m_reduced_reflected;}
	double diffuse_derate() {return m_diffuse_derate;}
	double reflected_derate() {return m_reflected_derate;}

	double m_X;
	double m_S;
	double m_Xe;
	double m_Hs;
	//double m_C1;
	//double m_C2;
	//double m_C3;
	//double m_C3_0;
	//double m_C4;
	double m_px;
	double m_py;
	double m_mask_angle;
	double m_tilt_eff;
	double m_d;
	double m_W;
	double m_L;
	double m_r;
	double m_A;
	double m_B;
	double m_m;
	double m_R;
	double m_n;
	double m_azimuth_eff;
	double m_zenith_eff;
	//double m_eqn5;
	//double m_eqn9;
	//double m_eqn10;
	//double m_eqn14;

private:
	ssarrdat m_arr;
	double m_dc_derate;
	double m_reduced_diffuse;
	double m_reduced_reflected;
	double m_diffuse_derate;
	double m_reflected_derate;

	void init();
	bool solar_transform(double solazi, double solzen);
	bool matrix_multiply(double a[][3], double b[][3], double c[][3]);
};

#endif
