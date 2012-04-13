#include "lib_pvshade.h"
#include "core.h"
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif
#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

static var_info _cm_vtab_test_pvshade[] = {

/*test and validation of lib_pvshade from sam_shading_type241.f90 */
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "ghi",				      "Global horizontal radiation",   "W/m2",  "",                      "pvshade",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dni",                   "Direct normal radiation","W/m2",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diffuse",                   "Diffuse radiation","W/m2",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sol_zenith",            "Solar Zenith","degrees",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sol_azimuth",            "Solar Azimuth","degrees",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },


	{ SSC_INPUT,        SSC_NUMBER,      "stc_fill_factor",			"Fill Factor at STC = Pmp0 / Voc0 / Isc0",	"",   "",                      "pvshade",             "*",						   "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "tilt",			"Tilt",	"degrees",   "",                      "pvshade",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",			"Azimuth",	"degrees",   "",                      "pvshade",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "length",			"module length",	"m",   "",                      "pvshade",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "width",			"module width",	"m",   "",                      "pvshade",             "*",						   "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "row_space",			"row spacing",	"m",   "",                      "pvshade",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mod_space",			"module spacing",	"m",   "",                      "pvshade",             "*",						   "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "slope_ns",			"Slope N/S",	"degrees",   "",                      "pvshade",             "*",						   "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "slope_ew",			"Slope E/W",	"degrees",   "",                      "pvshade",             "*",						   "",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "mod_orient",			"module orientation",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "str_orient",			"string orientation (wiring)",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "nmodx",			"modules in x",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "nmody",			"modules in y",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "nrows",			"rows of modules",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ncellx",			"cells per modules in x",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ncelly",			"cells per modules in y",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ndiode",			"diodes per modules",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },

	{ SSC_OUTPUT,        SSC_NUMBER,      "cf_length",			"output array length",	"",   "",                      "pvshade",             "*",						   "INTEGER",                              "" },

	{ SSC_OUTPUT,        SSC_ARRAY,       "shading_area",   "Shading area","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "shading_reduc",   "Shading reduction","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "azimuth_eff",   "Effective Azimuth","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "zenith_eff",   "Effective Zenith","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "xs",   "x to shaded area","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "px",   "x shadow","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "ys",   "y to shaded area","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "py",   "y shadow","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "eqn5",   "Equation 5 in paper from Chris Deline","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "eqn9",   "Equation 5 in paper from Chris Deline","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "eqn10",   "Equation 5 in paper from Chris Deline","",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=cf_length",                        "" },


var_info_invalid };


enum {
	CF_shading_area,
	CF_shading_reduc,
	CF_azimuth_eff,
	CF_zenith_eff,
	CF_xs,
	CF_px,
	CF_ys,
	CF_py,
	CF_eqn5,
	CF_eqn9,
	CF_eqn10,
	CF_max };



class cm_test_pvshade : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_test_pvshade()
	{
		add_var_info( _cm_vtab_test_pvshade );
	}

	void exec( ) throw( general_error )
	{
		size_t arr_len;
		ssc_number_t *p_ghi = as_array( "ghi", &arr_len );
		ssc_number_t *p_diffuse = as_array( "diffuse", &arr_len );
		ssc_number_t *p_sol_zenith = as_array( "sol_zenith", &arr_len );
		ssc_number_t *p_sol_azimuth = as_array( "sol_azimuth", &arr_len );


		ssarrdat p_ssarrdat;
		p_ssarrdat.azimuth = as_double("azimuth");
		p_ssarrdat.tilt = as_double("tilt");
		p_ssarrdat.length = as_double("length");
		p_ssarrdat.width = as_double("width");
		p_ssarrdat.row_space = as_double("row_space");
		p_ssarrdat.mod_space = as_double("mod_space");
		p_ssarrdat.slope_ns = as_double("slope_ns");
		p_ssarrdat.slope_ew = as_double("slope_ew");
		p_ssarrdat.mod_orient = as_integer("mod_orient");
		p_ssarrdat.str_orient = as_integer("str_orient");
		p_ssarrdat.nmodx = as_integer("nmodx");
		p_ssarrdat.nmody = as_integer("nmody");
		p_ssarrdat.nrows = as_integer("nrows");
		p_ssarrdat.ncellx = as_integer("ncellx");
		p_ssarrdat.ncelly = as_integer("ncelly");
		p_ssarrdat.ndiode = as_integer("ndiode");

		cf.resize_fill( CF_max, arr_len, 0.0 );


/*
	// test matrix maultiplication
    double Rx[3][3];
    double Ry[3][3];
    double Rz[3][3];

	Rx[0][0] = 1;
    Rx[0][1] = 2;
    Rx[0][2] = 3;
    Rx[1][0] = 4;
    Rx[1][1] = 5;
    Rx[1][2] = 6;
    Rx[2][0] = 7;
    Rx[2][1] = 8;
    Rx[2][2] = 9;

	Ry[0][0] = 1;
    Ry[0][1] = 2;
    Ry[0][2] = 3;
    Ry[1][0] = 4;
    Ry[1][1] = 5;
    Ry[1][2] = 6;
    Ry[2][0] = 7;
    Ry[2][1] = 8;
    Ry[2][2] = 9;

	ss.matrix_multiply( Rx, Ry, Rz );

	std::stringstream outm;
	outm  << "\n"<<  Rz[0][0] << "  " <<  Rz[0][1] << "  " <<  Rz[0][2];
	outm  << "\n"<<  Rz[1][0] << "  " <<  Rz[1][1] << "  " <<  Rz[1][2] ;
	outm  << "\n"<<  Rz[2][0] << "  " <<  Rz[2][1] << "  " <<  Rz[2][2] ;
	log( outm.str() );

	return;
*/


	std::stringstream outm;
	outm  << "\n azimuth="<< p_ssarrdat.azimuth;
	outm  << "\n tilt="<< p_ssarrdat.tilt;
	outm  << "\n length="<< p_ssarrdat.length;
	outm  << "\n width="<< p_ssarrdat.width;
	outm  << "\n row_space="<< p_ssarrdat.row_space;
	outm  << "\n mod_space="<< p_ssarrdat.mod_space;
	outm  << "\n slope_ns="<< p_ssarrdat.slope_ns;
	outm  << "\n slope_ew="<< p_ssarrdat.slope_ew;
	outm  << "\n mod_orient="<< p_ssarrdat.mod_orient;
	outm  << "\n str_orient="<< p_ssarrdat.str_orient;
	outm  << "\n nmodx="<< p_ssarrdat.nmodx;
	outm  << "\n nmody="<< p_ssarrdat.nmody;
	outm  << "\n nrows="<< p_ssarrdat.nrows;
	outm  << "\n ncellx="<< p_ssarrdat.ncellx;
	outm  << "\n ncelly="<< p_ssarrdat.ncelly;
	outm  << "\n ndiode="<< p_ssarrdat.ndiode;
	outm  << "\n";
	log( outm.str() );

		selfshade_t ss( p_ssarrdat );

		double FF0 = as_double("stc_fill_factor");

		for (int i=0;i<(int)arr_len;i++)
		{
			if ( ss.exec(  p_sol_zenith[i],p_sol_azimuth[i], p_ghi[i], p_diffuse[i], FF0 ) )
			{
				cf.at( CF_shading_area, i ) = ss.shade_area();
				cf.at( CF_shading_reduc, i ) = ss.dc_derate();
				cf.at( CF_azimuth_eff, i ) = ss.azimuth_eff;
				cf.at( CF_zenith_eff, i ) = ss.zenith_eff;
				cf.at( CF_px, i ) = ss.m_px;
				cf.at( CF_py, i ) = ss.m_py;
				cf.at( CF_eqn5, i ) = ss.m_eqn5;
				cf.at( CF_eqn9, i ) = ss.m_eqn9;
				cf.at( CF_eqn10, i ) = ss.m_eqn10;
			}
			else 
			{
				std::stringstream outm;
				outm <<  "Bad shading calculation at hour " << i ;
				log( outm.str() );
			}
		}

		assign( "cf_length", var_data( (ssc_number_t) arr_len ));

		save_cf( CF_shading_area, arr_len, "shading_area" );
		save_cf( CF_shading_reduc, arr_len, "shading_reduc" );
		save_cf( CF_azimuth_eff, arr_len, "azimuth_eff" );
		save_cf( CF_zenith_eff, arr_len, "zenith_eff" );
		save_cf( CF_xs, arr_len, "xs" );
		save_cf( CF_px, arr_len, "px" );
		save_cf( CF_ys, arr_len, "ys" );
		save_cf( CF_py, arr_len, "py" );
		save_cf( CF_eqn5, arr_len, "eqn5" );
		save_cf( CF_eqn9, arr_len, "eqn9" );
		save_cf( CF_eqn10, arr_len, "eqn10" );
	}


	void save_cf(int cf_line, int arr_len, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, arr_len );
		for (int i=0;i<arr_len;i++)
			arrp[i] = (ssc_number_t)cf.at(cf_line, i);
	}



	double min( double a, double b )
	{
		return (a < b) ? a : b;
	}

	double max( double a, double b )
	{
		return (a > b) ? a : b;
	}

};




DEFINE_MODULE_ENTRY( test_pvshade, "Test of lib_pvshade in recore", 1 );


