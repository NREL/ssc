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
	selfshade_t( ssarrdat &arr,
		double solzen,
		double solazi,
		double beamnorm,
		double globhoriz  );

	double dc_derate() {return m_dc_derate;}
	bool solar_transform(double &solazi, double &solzen, ssarrdat &arr);
	bool matrix_multiply(double a[][3], double b[][3], double c[][3]);

private:
	double m_dc_derate;
};



#endif
