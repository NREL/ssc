#include "lib_pvshade.h"
#include "core.h"
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif

static var_info _cm_vtab_test_pvshade[] = {

/*test and validation of lib_pvshade from sam_shading_type241.f90 */
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_ARRAY,       "ghi",				      "Global horizontal radiation",   "W/m2",  "",                      "pvshade",      "*",                       "",                                         "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dni",                   "Direct normal radiation","W/m2",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sol_zenith",            "Solar Zenith","Wdegreesm2",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sol_azimuth",            "Solar Azimuth","Wdegreesm2",  "",                      "pvshade",      "*",                       "LENGTH_EQUAL=ghi",                        "" },


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


var_info_invalid };


enum {
	CF_shading_area,
	CF_shading_reduc,
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
		ssc_number_t *p_dni = as_array( "dni", &arr_len );
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


		selfshade_t ss( p_ssarrdat );

		for (int i=0;i<(int)arr_len;i++)
		{
			if (ss.exec( p_sol_zenith[i], p_sol_azimuth[i], p_dni[i], p_ghi[i]) )
			{
				cf.at( CF_shading_area, i ) = ss.shade_area();
				cf.at( CF_shading_reduc, i ) = ss.dc_derate();
				if (ss.shade_area() < 0)
				{
				std::stringstream outm;
				outm <<  "hour " << i << ", shade area = " << ss.shade_area()  << ", xs = " << ss.m_xs  << ", ys = " << ss.m_ys  << ", lrows = " << ss.m_lrows  << ", wrows = " << ss.m_wrows ;
				log( outm.str() );
				}
			}
			else 
			{
				std::stringstream outm;
				outm <<  "Bad shading calculation at hour " << i ;
				log( outm.str() );
			}
		}

		assign( "cf_length", var_data( (ssc_number_t) arr_len+1 ));

		save_cf( CF_shading_area, arr_len, "shading_area" );
		save_cf( CF_shading_reduc, arr_len, "shading_reduc" );
	}


	void save_cf(int cf_line, int nyears, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, nyears+1 );
		for (int i=0;i<=nyears;i++)
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


