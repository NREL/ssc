#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include <shared/lib_util.h>
#include "shared/lib_weatherfile.h"
#include <algorithm>

#include "interpolation_routines.h"
#include "solarpilot/AutoPilot_API.h"
#include "solarpilot/IOUtil.h"
#include "solarpilot/sort_method.h"

using namespace std;

static bool solarpilot_callback( simulation_info *siminfo, void *data );

/* 
A self-contained heliostat field type that directly calls SolarPilot through the AutoPilot API. The user
may optionally specify the heliostat field positions or may use this type to generate positions based on
input parameters.

This type can be run in three different modes. 

----------------------------------------------------------------------------------------------------------------------
					Parameters
----------------------------------------------------------------------------------------------------------------------

--- RUN_TYPE::AUTO --- SolarPILOT automatic mode
	** Generate field layout and characterize performance with user-specified macro-geometry **
	#######################################################################################################
	No.	|	Item						|	Variable				|	Units	|	Note
	----- INPUTS ------------------------------------------------------------------------------------------
	0	|	Run type					|	run_type				|	-		|	enum RUN_TYPE
	1	|	Heliostat width				|	helio_width				|	m		|
	2	|	Heliostat height			|	helio_height			|	m		|
	3	|	Heliostat optical error		|	helio_optical_error		|	rad		|
	4	|	Heliostat active frac.		|	helio_active_fraction	|	-		|
	5	|	Heliostat reflectance		|	helio_reflectance		|	-		|
	6	|	Receiver absorptance		|	rec_absorptance			|	-		|
	7	|	Receiver height				|	rec_height				|	m		|
	8	|	Receiver aspect ratio		|	rec_aspect				|	- 		|	(H/W)
	9	|	Receiver design heatloss	|	rec_hl_perm2			|	kW/m2	|
	10	|	Field thermal power rating	|	q_design				|	kW		|
	11	|	Tower height				|	h_tower					|	m		|
	12	|	Weather file name			|	weather_file			|	-		|	String
	13	|	Land boundary type			|	land_bound_type			|	- 		|	(sp_layout::LAND_BOUND_TYPE)
	14	|	Land max boundary			|	land_max				|	- OR m	|	X tower heights OR fixed radius
	15	|	Land min boundary			|	land_min				|	- OR m	|	X tower heights OR fixed radius
	16	|	<>Land boundary table		|	land_bound_table		|	m		|	Polygon {{x1,y1},{x2,y2},...}
	17	|	<>Boundary table listing	|	land_bound_list			|	-		|	Poly. sizes (- if exclusion) {L1,-L2,...}
	18	|	Heliostat startup energy	|	p_start					|	kWe-hr	|	
	19	|	Heliostat tracking energy	|	p_track					|	kWe		|	
	20	|	Stow/deploy elevation		|	hel_stow_deploy			|	deg		|
	21	|	Max. wind velocity			|	v_wind_max				|	m/s		|
	22	|	<>Interpolation nugget		|	interp_nug				|	-		|
	23	|	<>Interpolation beta coef.	|	interp_beta				|	-		|
	24	|	Flux map X resolution		|	n_flux_x				|	-		|
	25	|	Flux map Y resolution		|	n_flux_y				|	-		|
	----- Parameters set on Init() ------------------------------------------------------------------------
	26	|	Heliostat position table	|	helio_positions			|	m		|	{{x1,y1,z1},...}
	27	|	<>Heliostat aim point table	|	helio_aim_points		|	m		|	{{x1,y1,z1},...} receiver coordinates
	28	|	Number of heliostats		|	N_hel					|	-		|
	29	|	Field efficiency array		|	eta_map					| deg,deg,-	|	{{az1,el1,eff1},{az2,...}}
	30	|	Flux map sun positions		|	flux_positions			| deg,deg	|	{{az1,el1},{az2,...}}
	31	|	Flux map intensities		|	flux_maps				|	-		|	{{f11,f12,f13...},{f21,f22...}..}
	########################################################################################################
	<> = Optional


--- RUN_TYPE::USER_FIELD --- SolarPILOT user-field mode
	** User specifies heliostat positions, macro geometry, annual performance characterized internally **
	#################  Required inputs ####################################################################
	No.	|	Item						|	Variable				|	Units	|	Note
	-------------------------------------------------------------------------------------------------------
	0	|	Run type					|	run_type				|	-		|	enum RUN_TYPE
	1	|	Heliostat width				|	helio_width				|	m		|
	2	|	Heliostat height			|	helio_height			|	m		|
	3	|	Heliostat optical error		|	helio_optical_error		|	rad		|
	4	|	Heliostat active frac.		|	helio_active_fraction	|	-		|
	5	|	Heliostat reflectance		|	helio_reflectance		|	-		|
	6	|	Receiver absorptance		|	rec_absorptance			|	-		|
	7	|	Receiver height				|	rec_height				|	m		|
	8	|	Receiver aspect ratio		|	rec_aspect				|	- 		|	(H/W)
	9	|	Receiver design heatloss	|	rec_hl_perm2			|	kW/m2	|
	10	|	Field thermal power rating	|	q_design				|	kW		|
	11	|	Tower height				|	h_tower					|	m		|
	12	|	Weather file name			|	weather_file			|	-		|	String
	13	|	Land boundary type			|	land_bound_type			|	- 		|	(sp_layout::LAND_BOUND_TYPE)
	14	|	Land max boundary			|	land_max				|	- OR m	|	X tower heights OR fixed radius
	15	|	Land min boundary			|	land_min				|	- OR m	|	X tower heights OR fixed radius
	16	|	<>Land boundary table		|	land_bound_table		|	m		|	Polygon {{x1,y1},{x2,y2},...}
	17	|	<>Boundary table listing	|	land_bound_list			|	-		|	Poly. sizes (- if exclusion) {L1,-L2,...}
	18	|	Heliostat startup energy	|	p_start					|	kWe-hr	|	
	19	|	Heliostat tracking energy	|	p_track					|	kWe		|	
	20	|	Stow/deploy elevation		|	hel_stow_deploy			|	deg		|
	21	|	Max. wind velocity			|	v_wind_max				|	m/s		|
	22	|	<>Interpolation nugget		|	interp_nug				|	-		|
	23	|	<>Interpolation beta coef.	|	interp_beta				|	-		|
	24	|	Flux map X resolution		|	n_flux_x				|	-		|
	25	|	Flux map Y resolution		|	n_flux_y				|	-		|
	26  |   Atm. atten coef 0           |   c_atm_0                 |   -       |
	27  |   Atm. atten coef 1           |   c_atm_1                 |   1/km    |
	28  |   Atm. atten coef 2           |   c_atm_2                 |   1/km^2  |
	29  |   Atm. atten coef 3           |   c_atm_3                 |   1/km^3  |
	30  |   Number of helio. facets X   |   n_facet_x               |   -       |
	31  |   Number of helio. facets Y   |   n_facet_y               |   -       |
	32  |   Helio. canting type         |   cant_type               |   -       |   0=Flat, 1=Ideal, 2=Equinox, 3=Summer sol., 4=Winter sol
	33  |   Helio. focus type           |   focus_type              |   -       |   0=Flat, 1=Ideal
	34	|	Heliostat position table	|	helio_positions			|	m		|	{{x1,y1,z1},...}
	35	|	<>Heliostat aim point table	|	helio_aim_points		|	m		|	{{x1,y1,z1},...} receiver coordinates
	----- Parameters set on Init() ------------------------------------------------------------------------
	36	|	<>Heliostat aim point table	|	helio_aim_points		|	m		|	{{x1,y1,z1},...} receiver coordinates
	37	|	Number of heliostats		|	N_hel					|	-		|
	38	|	Field efficiency array		|	eta_map					| deg,deg,-	|	{{az1,el1,eff1},{az2,...}}
	39	|	Flux map sun positions		|	flux_positions			| deg,deg	|	{{az1,el1},{az2,...}}
	40	|	Flux map intensities		|	flux_maps				|	-		|	{{f11,f12,f13...},{f21,f22...}..}
	########################################################################################################
	<> = Optional

--- RUN_TYPE::USER_DATA --- User-data mode
	** User specifies field efficiency and flux intensity on the receiver vs. sun position. No SolarPILOT runs **
	#################  Required inputs ####################################################################
	No.	|	Item						|	Variable				|	Units	|	Note
	-------------------------------------------------------------------------------------------------------
	0	|	Run type					|	run_type				|	-		|	enum RUN_TYPE
	11	|	Tower height				|	h_tower					|	m		|
	12	|	Weather file name			|	weather_file			|	-		|	String
	13	|	Land boundary type			|	land_bound_type			|	- 		|	(sp_layout::LAND_BOUND_TYPE)
	14	|	Land max boundary			|	land_max				|	- OR m	|	X tower heights OR fixed radius
	15	|	Land min boundary			|	land_min				|	- OR m	|	X tower heights OR fixed radius
	16	|	<>Land boundary table		|	land_bound_table		|	m		|	Polygon {{x1,y1},{x2,y2},...}
	17	|	<>Boundary table listing	|	land_bound_list			|	-		|	Poly. sizes (- if exclusion) {L1,-L2,...}
	18	|	Heliostat startup energy	|	p_start					|	kWe-hr	|	
	19	|	Heliostat tracking energy	|	p_track					|	kWe		|	
	20	|	Stow/deploy elevation		|	hel_stow_deploy			|	deg		|
	21	|	Max. wind velocity			|	v_wind_max				|	m/s		|
	22	|	<>Interpolation nugget		|	interp_nug				|	-		|
	23	|	<>Interpolation beta coef.	|	interp_beta				|	-		|
	24	|	Flux map X resolution		|	n_flux_x				|	-		|
	25	|	Flux map Y resolution		|	n_flux_y				|	-		|
	28	|	Number of heliostats		|	N_hel					|	-		|
	29	|	Field efficiency array		|	eta_map					| deg,deg,-	|	{{az1,el1,eff1},{az2,...}}
	30	|	Flux map sun positions		|	flux_positions			| deg,deg	|	{{az1,el1},{az2,...}}
	31	|	Flux map intensities		|	flux_maps				|	-		|	{{f11,f12,f13...},{f21,f22...}..}
	32  |   Atm. atten coef 0           |   c_atm_0                 |   -       |
	33  |   Atm. atten coef 1           |   c_atm_1                 |   1/km    |
	34  |   Atm. atten coef 2           |   c_atm_2                 |   1/km^2  |
	35  |   Atm. atten coef 3           |   c_atm_3                 |   1/km^3  |
	36  |   Number of helio. facets X   |   n_facet_x               |   -       |
	37  |   Number of helio. facets Y   |   n_facet_y               |   -       |
	38  |   Helio. canting type         |   cant_type               |   -       |   0=Flat, 1=Ideal, 2=Equinox, 3=Summer sol., 4=Winter sol
	39  |   Helio. focus type           |   focus_type              |   -       |   0=Flat, 1=Ideal
	----- Parameters set on Init() ------------------------------------------------------------------------
	None
	########################################################################################################
	<> = Optional
	

Note: 
Annual efficiency is generated using non-uniform sun position spacing. The method implemented to handle
interpolation of non-uniform data is Gauss-Markov estimation (Kriging), with parameters set to induce
nearly linear interpolation that maintains fit fidelity with the original data.

*/


enum{	//Parameters
		P_run_type, 
		P_helio_width, 
		P_helio_height, 
		P_helio_optical_error, 
		P_helio_active_fraction, 
		P_helio_reflectance, 
		P_rec_absorptance, 
		P_rec_height, 
		P_rec_aspect, 
		P_rec_hl_perm2, 
		P_q_design, 
		P_h_tower, 
		P_weather_file,
		P_land_bound_type, 
		P_land_max, 
		P_land_min, 
		P_land_bound_table, 
		P_land_bound_list, 
		P_p_start, 
		P_p_track, 
		P_hel_stow_deploy, 
		P_v_wind_max, 
		P_interp_nug, 
		P_interp_beta, 
		P_n_flux_x, 
		P_n_flux_y, 
		P_helio_positions, 
		P_helio_aim_points, 
		P_N_hel, 
		P_eta_map, 
		P_flux_positions, 
		P_flux_maps,
		P_c_atm_0,
		P_c_atm_1,
		P_c_atm_2,
		P_c_atm_3,
		P_n_facet_x,
		P_n_facet_y,
		P_cant_type,
		P_focus_type,
		P_n_flux_days,
		P_delta_flux_hrs,
		P_dni_des,

		//Inputs
		I_v_wind,
		I_field_control,
		I_solaz,
		I_solzen,

		//Outputs
		O_pparasi,
		O_eta_field,
		O_flux_map,

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_heliostatfield_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER,	P_run_type,				"run_type",					"Run type",					"-",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_helio_width,			"helio_width",				"Heliostat width",			"m",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_helio_height,			"helio_height",				"Heliostat height",			"m",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_helio_optical_error,	"helio_optical_error",		"Heliostat optical error",	"rad",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_helio_active_fraction, "helio_active_fraction",	"Heliostat active frac.",	"-",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_helio_reflectance,	"helio_reflectance",		"Heliostat reflectance",	"-",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_rec_absorptance,		"rec_absorptance",			"Receiver absorptance",		"-",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_rec_height,			"rec_height",				"Receiver height", "m",		"",		"", ""},
	{TCS_PARAM, TCS_NUMBER,	P_rec_aspect,			"rec_aspect",				"Receiver aspect ratio",	"- ",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_rec_hl_perm2,			"rec_hl_perm2",				"Receiver design heatloss", "kW/m2", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_q_design,				"q_design",					"Field thermal power rating", "kW",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_h_tower,				"h_tower",					"Tower height",				"m", "", "", ""},
	{TCS_PARAM, TCS_STRING,	P_weather_file,			"weather_file",				"Weather file location",	"-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_land_bound_type,		"land_bound_type",			"Land boundary type",		"- ", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_land_max,				"land_max",					"Land max boundary",		"- OR m", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_land_min,				"land_min",					"Land min boundary",		"- OR m", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_land_bound_table,		"land_bound_table",			"Land boundary table",		"m", "", "", ""},
	{TCS_PARAM, TCS_ARRAY,	P_land_bound_list,		"land_bound_list",			"Boundary table listing",	"-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_p_start,				"p_start",					"Heliostat startup energy", "kWe-hr", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_p_track,				"p_track",					"Heliostat tracking energy", "kWe", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_hel_stow_deploy,		"hel_stow_deploy",			"Stow/deploy elevation",	"deg", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_v_wind_max,			"v_wind_max",				"Max. wind velocity",		"m/s", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_interp_nug,			"interp_nug",				"Interpolation nugget",		"-", "", "", "0.0"},
	{TCS_PARAM, TCS_NUMBER,	P_interp_beta,			"interp_beta",				"Interpolation beta coef.", "-", "", "", "1.99"},
	{TCS_PARAM, TCS_NUMBER,	P_n_flux_x,				"n_flux_x",					"Flux map X resolution",	"-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_n_flux_y,				"n_flux_y",					"Flux map Y resolution",	"-", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_helio_positions,		"helio_positions",			"Heliostat position table", "m", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_helio_aim_points,		"helio_aim_points",			"Heliostat aim point table", "m", "", "", ""},
	{TCS_PARAM, TCS_NUMBER,	P_N_hel,				"N_hel",					"Number of heliostats",		"-", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_eta_map,				"eta_map",					"Field efficiency array",	"-", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_flux_positions,		"flux_positions",			"Flux map sun positions",	"deg", "", "", ""},
	{TCS_PARAM, TCS_MATRIX,	P_flux_maps,			"flux_maps",				"Flux map intensities",		"-", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_c_atm_0,				"c_atm_0",					"Attenuation coefficient 0", "", "", "", "0.006789"},
	{TCS_PARAM, TCS_NUMBER, P_c_atm_0,				"c_atm_1",					"Attenuation coefficient 1", "", "", "", "0.1046"},
	{TCS_PARAM, TCS_NUMBER, P_c_atm_0,				"c_atm_2",					"Attenuation coefficient 2", "", "", "", "-0.0107"},
	{TCS_PARAM, TCS_NUMBER, P_c_atm_0,				"c_atm_3",					"Attenuation coefficient 3", "", "", "", "0.002845"},
	{TCS_PARAM, TCS_NUMBER, P_n_facet_x,            "n_facet_x",                "Number of heliostat facets - X", "", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_facet_y,            "n_facet_y",                "Number of heliostat facets - Y", "", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_cant_type,            "cant_type",                "Heliostat cant method",    "", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_focus_type,           "focus_type",               "Heliostat focus method", "", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_flux_days,          "n_flux_days",              "No. days in flux map lookup", "", "", "", "8"},
	{TCS_PARAM, TCS_NUMBER, P_delta_flux_hrs,       "delta_flux_hrs",           "Hourly frequency in flux map lookup", "hrs", "", "", "1"},
	{TCS_PARAM, TCS_NUMBER, P_dni_des,              "dni_des",                  "Design-point DNI", "W/m2", "", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_v_wind,			"vwind",			"Wind velocity",										"m/s",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_field_control,	"field_control",	"Field defocus control",								"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_solaz,			"solaz",			"Solar azimuth angle: 0 due north, clockwise to +360",	"deg",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_solzen,			"solzen",			"Solar zenith angle",									"deg",	    "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_pparasi,			"pparasi",			"Parasitic tracking/startup power",						"MWe",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_eta_field,		"eta_field",		"Total field efficiency",								"",			"", "", ""},
	{TCS_OUTPUT, TCS_MATRIX, O_flux_map,		"flux_map",			"Receiver flux map",									"",			"n_flux_x cols x n_flux_y rows", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	}
};

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif


#define pi 3.141592654
#define az_scale 6.283125908 
#define zen_scale 1.570781477 
#define eff_scale 0.7

class sam_mw_pt_heliostatfield : public tcstypeinterface
{
private:
	// Class Instances
	GaussMarkov *field_efficiency_table;
	// Flux table
	sp_flux_table fluxtab;

	//Parameters
	string weather_file;
	int run_type;
	double helio_width;
	double helio_height;
	double helio_optical_error;
	double helio_active_fraction;
	double helio_reflectance;
	double rec_absorptance;
	double rec_height;
	double rec_aspect;
	double rec_hl_perm2;
	double q_design;
	double h_tower;
	int land_bound_type;
	double land_max;
	double land_min;
	double* land_bound_table;
	double* land_bound_list;
	double p_start;
	double p_track;
	double hel_stow_deploy;
	double v_wind_max;
	double interp_nug;
	double interp_beta;
	double* helio_positions;
	double* helio_aim_points;
	int N_hel, pos_dim;
	double* eta_map;
	int n_flux_x;
	int n_flux_y;
	double* flux_positions;
	double* flux_maps;
	double* flux_map;
	double c_atm_0, c_atm_1, c_atm_2, c_atm_3;
	int n_facet_x, n_facet_y;
	int cant_type, focus_type;
	int n_flux_days, delta_flux_hrs;
	double dni_des;
	
	//Stored Variables
	double eta_prev;
	double v_wind_prev;

public:

	struct RUN_TYPE { enum A {AUTO, USER_FIELD, USER_DATA}; };

	sam_mw_pt_heliostatfield( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		run_type	=0;
		helio_width	=std::numeric_limits<double>::quiet_NaN();
		helio_height	=std::numeric_limits<double>::quiet_NaN();
		helio_optical_error	=std::numeric_limits<double>::quiet_NaN();
		helio_active_fraction	=std::numeric_limits<double>::quiet_NaN();
		helio_reflectance	=std::numeric_limits<double>::quiet_NaN();
		rec_absorptance	=std::numeric_limits<double>::quiet_NaN();
		rec_height	=std::numeric_limits<double>::quiet_NaN();
		rec_aspect	=std::numeric_limits<double>::quiet_NaN();
		rec_hl_perm2	=std::numeric_limits<double>::quiet_NaN();
		q_design	=std::numeric_limits<double>::quiet_NaN();
		h_tower	=std::numeric_limits<double>::quiet_NaN();
		land_bound_type	=0;
		land_max	=std::numeric_limits<double>::quiet_NaN();
		land_min	=std::numeric_limits<double>::quiet_NaN();
		land_bound_table	= NULL;
		land_bound_list	= NULL;
		p_start	=std::numeric_limits<double>::quiet_NaN();
		p_track	=std::numeric_limits<double>::quiet_NaN();
		hel_stow_deploy	=std::numeric_limits<double>::quiet_NaN();
		v_wind_max	=std::numeric_limits<double>::quiet_NaN();
		interp_nug	=std::numeric_limits<double>::quiet_NaN();
		interp_beta	=std::numeric_limits<double>::quiet_NaN();
		helio_positions	= NULL;
		helio_aim_points	= NULL;
		N_hel	=0;
		pos_dim = 0;
		eta_map	= NULL;
		n_flux_x	=0;
		n_flux_y	=0;
		flux_positions	= NULL;
		flux_maps	= NULL;
		flux_map = NULL;
		n_facet_x = 0;
		n_facet_y = 0;
		cant_type = 0;
		focus_type = 0;
		n_flux_days = 0;
		delta_flux_hrs = 0;
		
		eta_prev = std::numeric_limits<double>::quiet_NaN();
		v_wind_prev = std::numeric_limits<double>::quiet_NaN();
		c_atm_0 = std::numeric_limits<double>::quiet_NaN();
		c_atm_1 = std::numeric_limits<double>::quiet_NaN();
		c_atm_2 = std::numeric_limits<double>::quiet_NaN();
		c_atm_3 = std::numeric_limits<double>::quiet_NaN();
		dni_des = std::numeric_limits<double>::quiet_NaN();

	}

	virtual ~sam_mw_pt_heliostatfield()
	{
		delete field_efficiency_table;
	}

	virtual int init()
	{

		//Read in parameters
		int nrows1, ncols1;
		int nrows2;
		int nrows4, ncols4;
		int nrows5, ncols5;
		int nrows6, ncols6;
		int nrows7, ncols7;

		run_type = (int)value(P_run_type);
		

		//Read in only those parameters that are relevant to the run scheme
		switch (run_type)
		{
		case sam_mw_pt_heliostatfield::RUN_TYPE::AUTO:
		case sam_mw_pt_heliostatfield::RUN_TYPE::USER_FIELD:
			helio_width = value(P_helio_width);
			helio_height = value(P_helio_height);
			helio_optical_error = value(P_helio_optical_error);
			helio_active_fraction = value(P_helio_active_fraction);
			helio_reflectance = value(P_helio_reflectance);
			rec_absorptance = value(P_rec_absorptance);
			rec_height = value(P_rec_height);
			rec_aspect = value(P_rec_aspect);
			rec_hl_perm2 = value(P_rec_hl_perm2);
			q_design = value(P_q_design);
			h_tower = value(P_h_tower);
			weather_file = value_str(P_weather_file);
			land_bound_type = (int)value(P_land_bound_type);
			land_max = value(P_land_max);
			land_min = value(P_land_min);
			land_bound_table = value(P_land_bound_table, &nrows1, &ncols1);
			land_bound_list = value(P_land_bound_list, &nrows2);
			p_start = value(P_p_start);
			p_track = value(P_p_track);
			hel_stow_deploy = value(P_hel_stow_deploy)*pi/180.;
			v_wind_max = value(P_v_wind_max);
			interp_nug = value(P_interp_nug);
			interp_beta = value(P_interp_beta);
			n_flux_x = (int)value(P_n_flux_x);
			n_flux_y = (int)value(P_n_flux_y);
			c_atm_0 = value(P_c_atm_0);
			c_atm_1 = value(P_c_atm_1);
			c_atm_2 = value(P_c_atm_2);
			c_atm_3 = value(P_c_atm_3);
			n_facet_x = (int)value(P_n_facet_x);
			n_facet_y = (int)value(P_n_facet_y);
			cant_type = (int)value(P_cant_type);
			focus_type = (int)value(P_focus_type);
			n_flux_days = (int)value(P_n_flux_days);
			delta_flux_hrs = (int)value(P_delta_flux_hrs);
			dni_des = value(P_dni_des);

			pos_dim = 2;	//initiaize with 2 dimensions (x,y) on helio positions
			if( run_type != sam_mw_pt_heliostatfield::RUN_TYPE::USER_FIELD ) break;

			helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
			helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
			
			break;
		case sam_mw_pt_heliostatfield::RUN_TYPE::USER_DATA:

			h_tower = value(P_h_tower);
			land_bound_type = (int)value(P_land_bound_type);
			land_max = value(P_land_max);
			land_min = value(P_land_min);
			land_bound_table = value(P_land_bound_table, &nrows1, &ncols1);
			land_bound_list = value(P_land_bound_list, &nrows2);
			p_start = value(P_p_start);
			p_track = value(P_p_track);
			hel_stow_deploy = value(P_hel_stow_deploy)*pi/180.;
			v_wind_max = value(P_v_wind_max);
			interp_nug = value(P_interp_nug);
			interp_beta = value(P_interp_beta);
			helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
			helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
			//N_hel = (int)value(P_N_hel);
			eta_map = value(P_eta_map, &nrows5, &ncols5);
			n_flux_x = (int)value(P_n_flux_x);
			n_flux_y = (int)value(P_n_flux_y);
			flux_positions = value(P_flux_positions, &nrows6, &ncols6);
			flux_maps = value(P_flux_maps, &nrows7, &ncols7);
			c_atm_0 = value(P_c_atm_0);
			c_atm_1 = value(P_c_atm_1);
			c_atm_2 = value(P_c_atm_2);
			c_atm_3 = value(P_c_atm_3);
			n_facet_x = (int)value(P_n_facet_x);
			n_facet_y = (int)value(P_n_facet_y);
			cant_type = (int)value(P_cant_type);
			focus_type = (int)value(P_focus_type);
			n_flux_days = (int)value(P_n_flux_days);
			delta_flux_hrs = (int)value(P_delta_flux_hrs);
			dni_des = value(P_dni_des);
			break;
		default:
			break;
		}

		MatDoub sunpos;
		vector<double> effs;

		//do initial runs of SolarPILOT and/or set up tables
		switch (run_type)
		{
		case sam_mw_pt_heliostatfield::RUN_TYPE::AUTO:
		case sam_mw_pt_heliostatfield::RUN_TYPE::USER_FIELD:
		{
			AutoPilot_S sapi;

			sp_optimize opt;
			sp_ambient amb;
			sp_cost cost;
			sp_heliostats helios;
			sp_receivers recs;
			sp_layout layout;
	
			var_set V;
			ioutil::parseDefinitionArray(V);
	
			// define stuff and load default values
			opt.LoadDefaults(V);
			amb.LoadDefaults(V);
			cost.LoadDefaults(V);
			helios.resize(1);
			helios.front().LoadDefaults(V);
			recs.resize(1);
			recs.front().LoadDefaults(V);
			layout.LoadDefaults(V);

			helios.front().width = helio_width;
			helios.front().height = helio_height;
			helios.front().optical_error = helio_optical_error;
			helios.front().active_fraction = helio_active_fraction;
			helios.front().reflectance = helio_reflectance;
			int cmap[5];
			cmap[0] = sp_heliostat::CANT_TYPE::FLAT;
			cmap[1] = sp_heliostat::CANT_TYPE::AT_SLANT;
			cmap[2] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
			cmap[3] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
			cmap[4] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
			helios.front().cant_type = cmap[ cant_type ];
			if( cant_type == 2 ){
				helios.front().cant_settings.point_day = 81;  //spring equinox
				helios.front().cant_settings.point_hour = 12.;
			}
			else if( cant_type == 3 ){
				helios.front().cant_settings.point_day = 172;  //Summer solstice
				helios.front().cant_settings.point_hour = 12.;
			}
			else if( cant_type == 4){
				helios.front().cant_settings.point_day = 355;  //Winter solstice
				helios.front().cant_settings.point_hour = 12.;
			}

			int fmap[2];
			fmap[0] = sp_heliostat::FOCUS_TYPE::FLAT;
			fmap[1] = sp_heliostat::FOCUS_TYPE::AT_SLANT;
			helios.front().focus_type = fmap[ focus_type ];

			recs.front().absorptance = rec_absorptance;
			recs.front().height = rec_height;
			recs.front().aspect = rec_aspect;
			recs.front().q_hl_perm2 = rec_hl_perm2;
			
			layout.q_design = q_design;
			layout.dni_design = dni_des;
			layout.land_max = land_max;
			layout.land_min = land_min;
			layout.h_tower = h_tower;

			//set up the weather data for simulation
			const char *wffile = weather_file.c_str();
			if ( !wffile ) message( "solarpilot: no weather file specified" );
			weatherfile wf( wffile );
			if ( !wf.ok() || wf.type() == weatherfile::INVALID ) message("solarpilot: could not open weather file or invalid weather file format");


			amb.site_latitude = wf.lat;
			amb.site_longitude = wf.lon;
			amb.site_time_zone = wf.tz;
			amb.atten_model = sp_ambient::ATTEN_MODEL::USER_DEFINED;
			amb.user_atten_coefs.clear();
			amb.user_atten_coefs.push_back(c_atm_0);
			amb.user_atten_coefs.push_back(c_atm_1);
			amb.user_atten_coefs.push_back(c_atm_2);
			amb.user_atten_coefs.push_back(c_atm_3);

			if(run_type == sam_mw_pt_heliostatfield::RUN_TYPE::AUTO)
			{
				/* 
				Generate the heliostat field layout using the settings provided by the user				
				*/
				vector<string> wfdata;
				wfdata.reserve( 8760 );
				char buf[1024];
				for( int i=0;i<8760;i++ )
				{
					if( !wf.read() ){
						string msg = "solarpilot: could not read data line " + util::to_string(i+1) + " of 8760 in weather file";
						message(msg.c_str());
					}

					mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
					wfdata.push_back( std::string(buf) );
				}

				sapi.SetDetailCallback( solarpilot_callback, (void*)this);
				sapi.SetSummaryCallbackStatus(false);

				sapi.GenerateDesignPointSimulations( amb, V, wfdata );
	
				sapi.Setup(amb, cost, layout, helios, recs);

				sapi.CreateLayout();


				//Copy the heliostat field positions into the 'helio_positions' data structure
				N_hel = (int)layout.heliostat_positions.size();
				helio_positions = allocate(P_helio_positions, N_hel, pos_dim);
				for( int i=0; i<N_hel; i++){
					TCS_MATRIX_INDEX( var(P_helio_positions), i, 0 ) = layout.heliostat_positions.at(i).location.x;
					TCS_MATRIX_INDEX( var(P_helio_positions), i, 1 ) = layout.heliostat_positions.at(i).location.y;
					if(pos_dim==3)
						TCS_MATRIX_INDEX( var(P_helio_positions), i, 2) = layout.heliostat_positions.at(i).location.z;
				}
				
				//update the callbacks
				sapi.SetDetailCallbackStatus(false);
				
			}
			else{

				/* 
				Load in the heliostat field positions that are provided by the user.
				*/
				layout.heliostat_positions.clear();
				layout.heliostat_positions.resize(N_hel);
				
				for( int i=0; i<N_hel; i++){

					layout.heliostat_positions.at(i).location.x = TCS_MATRIX_INDEX( var(P_helio_positions), i, 0 );
					layout.heliostat_positions.at(i).location.y = TCS_MATRIX_INDEX( var(P_helio_positions), i, 1 );
					if(pos_dim==3)
						layout.heliostat_positions.at(i).location.z = TCS_MATRIX_INDEX( var(P_helio_positions), i, 2);
					
				}

				sapi.Setup(amb, cost, layout, helios, recs);
								
			}
			value(P_N_hel, (double)N_hel);

			sapi.SetSummaryCallbackStatus(true);
			sapi.SetSummaryCallback( solarpilot_callback, (void*)this);

			//set up flux map resolution
			fluxtab.is_user_spacing = true;
			fluxtab.n_flux_days = n_flux_days;
			fluxtab.delta_flux_hrs = delta_flux_hrs;

			//run the flux maps
			sapi.CalculateFluxMaps(fluxtab, n_flux_x, n_flux_y, true);

			//collect efficiencies
			sunpos.clear();
			effs.clear();
			int npos = (int)fluxtab.azimuths.size();
			sunpos.reserve(npos);
			effs.reserve(npos);
			for(int i=0; i<npos; i++){
				sunpos.push_back( vector<double>(2, 0.) );

				sunpos.back().at(0) = fluxtab.azimuths.at(i) / az_scale;
				sunpos.back().at(1) = fluxtab.zeniths.at(i) / zen_scale;
				effs.push_back( fluxtab.efficiency.at(i) / eff_scale );
			}

			//collect flux's
			flux_maps = allocate( P_flux_maps, n_flux_y * npos, n_flux_x );
			//size the output
			flux_map = allocate( O_flux_map, n_flux_y, n_flux_x );

			block_t<double> *f = &fluxtab.flux_surfaces.front().flux_data;

			int nfl = f->nlayers();

			for(int i=0; i<nfl; i++){
				for(int j=0; j<n_flux_y; j++){
					for(int k=0; k<n_flux_x; k++){
						TCS_MATRIX_INDEX( var(P_flux_maps), i*n_flux_y + j, k) = f->at(j, k, i);
					}
				}
			}

			break;
		}
		case sam_mw_pt_heliostatfield::RUN_TYPE::USER_DATA:
		{

			int nrows, ncols;
			double *p_map = value( P_eta_map, &nrows, &ncols);
		
			if(ncols != 3){
				message("The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
					" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);
				return -1;
			}
		
			//read the data from the array into the local storage arrays
			sunpos.resize(nrows, VectDoub(2));
			effs.resize(nrows);
			for(int i=0; i<nrows; i++){
				sunpos.at(i).at(0) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 0 ) / az_scale;
				sunpos.at(i).at(1) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 1 ) / zen_scale;
				effs.at(i) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 2 ) / eff_scale;
			}

			
			break;
		}
		default:
			break;
		}

		/* 
		------------------------------------------------------------------------------
		Create the regression fit on the efficiency map
		------------------------------------------------------------------------------
		*/
		
		//collect nug and beta
		interp_beta = value(P_interp_beta);
		interp_nug = value(P_interp_nug);

		//Create the field efficiency table
		Powvargram vgram(sunpos, effs, interp_beta, interp_nug);
		field_efficiency_table = new GaussMarkov(sunpos, effs, vgram);

		//test how well the fit matches the data
		double err_fit = 0.;
		int npoints = (int)sunpos.size();
		for(int i=0; i<npoints; i++){
			double zref = effs.at(i);
			double zfit = field_efficiency_table->interp( sunpos.at(i) );
			double dz = zref - zfit;
			err_fit += dz * dz;
		}
		err_fit = sqrt(err_fit);
		if( err_fit > 0.01 )
			message("The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);

		// Initialize stored variables
		eta_prev = 0.0;
		v_wind_prev = 0.0;

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{						
		// GET AND CHECK INPUT VALUES
		double v_wind = value( I_v_wind );	// [m/s] wind speed
		double field_control = value( I_field_control ); // Control Parameter ( range from 0 to 1; 0=off, 1=all on)
		if( field_control > 1.0 )
			field_control = 1.0;
		if( field_control < 0.0 )
			field_control = 0.0;
		double solzen = value( I_solzen )*pi/180.;	// solar zenith angle 
		if( solzen >= pi/2. )
			field_control = 0.0;		// No tracking before sunrise of after sunset
		double solaz = value( I_solaz )*pi/180.;	
		
		//clear out the existing flux map
		for(int j=0; j<n_flux_y; j++)
			for(int i=0; i<n_flux_x; i++)
				TCS_MATRIX_INDEX( var(O_flux_map), j, i) = 0.;
      
		// Parasitics for startup or shutdown
		double pparasi = 0.0; 
		
		// If starting up or shutting down, calculate parasitics
		if( (field_control > 1.e-4 && eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= v_wind_max ) ||		// OR Shutdown by high wind speed
		(eta_prev > 1.e-4 && v_wind_prev >= v_wind_max && v_wind < v_wind_max)  )	// OR Startup after high wind speed
			pparasi = N_hel * p_start / (step/3600.0);			// kJ/hr 
     
		// Parasitics for tracking      
		if( v_wind < v_wind_max && v_wind_prev < v_wind_max )
				pparasi += N_hel * p_track * field_control;	// kJ/hr

		double eta_field = 0.;

		if( solzen > (pi/2 - .001 - hel_stow_deploy) || v_wind > v_wind_max || time < 3601){
			eta_field = 1.e-6;
		}
		else{
				
			// Use current solar position to interpolate field efficiency table and fied solar field efficiency
			vector<double> sunpos;
			sunpos.push_back(solaz/az_scale);
			sunpos.push_back(solzen/zen_scale);

			eta_field = field_efficiency_table->interp( sunpos ) * eff_scale;
			eta_field = min( max ( eta_field, 0.0 ), 1.0 ) * field_control;		// Ensure physical behavior 

			//Set the active flux map
			MatDoub *map_sunpos = &field_efficiency_table->x;
			VectDoub pos_now(sunpos);
			/*VectDoub pos_now(2);
			pos_now.at(0) = solaz/az_scale;
			pos_now.at(1) = solzen/zen_scale;*/
			//find the nearest neighbors to the current point
			vector<double> distances;
			vector<int> indices;
			for(int i=0; i<(int)map_sunpos->size(); i++){
				distances.push_back( rdist( & pos_now, &map_sunpos->at(i) ) );
				indices.push_back( i );
			}
			quicksort<double,int>( distances, indices );
			//calculate weights for the nearest 6 points
			double avepoints = 0.;
			const int npt = 6;
			for(int i=0; i<npt; i++)
				avepoints += distances.at(i);
			avepoints *= 1./(double)npt;
			VectDoub weights(npt);
			double normalizer = 0.;
			for(int i=0; i<npt; i++){
				double w = exp( - pow(distances.at(i)/avepoints, 2) );
				weights.at(i) = w;
				normalizer += w;
			}
			for(int i=0; i<npt; i++)
				weights.at(i) *= 1./normalizer;

			//set the values
			for(int k=0; k<npt; k++){
				int imap = indices.at(k);
				for(int j=0; j<n_flux_y; j++){
					for(int i=0; i<n_flux_x; i++){
						TCS_MATRIX_INDEX( var(O_flux_map), j, i) += 
							TCS_MATRIX_INDEX( var(P_flux_maps), imap*n_flux_y + j, i ) * weights.at(k);
					}
				}
			}
			
		}
		
		
		// Set output parameters
		value( O_pparasi, pparasi/3.6e6 );	// [MW], convert from kJ/hr: Parasitic power for tracking
		value( O_eta_field, eta_field );	// [-], field efficiency
		
		return 0;
	}

	virtual int converged( double time )
	{
		eta_prev = value( O_eta_field );
		v_wind_prev = value( I_v_wind );
		
		return 0;
	}

	int relay_message( string &msg, double progress ){
		message("SolarPILOT progress (%s): %.1f%s\n", msg.c_str(), progress, "%");
		return 0;
	}

	double rdist(VectDoub *p1, VectDoub *p2, int dim=2){
		double d=0;
		for(int i=0; i<dim; i++){
			double rd = p1->at(i) - p2->at(i);
			d += rd * rd;
		}
		return sqrt(d);
	}

};

static bool solarpilot_callback( simulation_info *siminfo, void *data )
{
	sam_mw_pt_heliostatfield *cm = static_cast<sam_mw_pt_heliostatfield*>( data );
	if ( !cm ) return false;
	float simprogress = (float)siminfo->getCurrentSimulation()/(float)(max(siminfo->getTotalSimulationCount(),1));
	
	return cm->relay_message( *siminfo->getSimulationNotices(), simprogress*100.0f ) == 0;
}

TCS_IMPLEMENT_TYPE( sam_mw_pt_heliostatfield, "Heliostat field with SolarPILOT", "Mike Wagner", 1, sam_mw_pt_heliostatfield_variables, NULL, 1 )

