#ifndef __pvshade_h
#define __pvshade_h

#include <string>


//   Porting of sam_shading_type241.f90 to new orientation


struct ssarrdat
{
	double tilt, azimuth;
	int nmodx, nmody, nrows;
	double length, width;
	int mod_orient, str_orient;
	double row_space, mod_space;
	int ncellx, ncelly, ndiode;
	double slope_ns, slope_ew;
};



class selfshade_t
{
public:
	selfshade_t();
	selfshade_t( ssarrdat &arr );

	bool exec( 
		double solzen,    // solar zenith
		double solazi,    // solar azimuth
		double beamnorm,  // dni
		double globhoriz, // ghi
		double diffuse,   // diffuse for D/G ratio
		double FF0);       // Fill Factor at STC = Pmp0 / Voc0 / Isc0;

	void set_ssarrdat( ssarrdat &arr ) {m_arr = arr;}
	double dc_derate() {return m_dc_derate;}
	double shade_area() {return m_shade_area;}
	bool solar_transform(double solazi, double solzen);

	bool matrix_multiply(double a[][3], double b[][3], double c[][3]);


// testing
	double m_X;
	double m_S;
	double m_fsub_fs;
	double m_fsub_ps;
	int	m_nstr;
	int	m_nstr_s;
	int m_nstr_fs;
	int m_nstr_ps;
	int m_nstr_us;
	double m_xs;
	double m_ys;
	double m_px;
	double m_py;
	double m_wrows;
	double m_lrows;
	double m_azi_eff;
	double m_zen_eff;
	double azimuth_eff;
	double zenith_eff;
	double m_eqn5;
	double m_eqn9;
	double m_eqn10;

private:
	ssarrdat m_arr;
	double m_dc_derate;
	double m_shade_area;
};



#endif
