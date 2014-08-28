#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include <shared/lib_util.h>
#include <algorithm>

#include "interpolation_routines.h"

using namespace std;

/* 

Allow interpolation of efficiency table data using non-uniform spacing. The method implemented to handle
interpolation of non-uniform data is Gauss-Markov estimation (Kriging), with parameters set to induce
nearly linear interpolation that maintains fit fidelity with the original data.

*/


enum{	//Parameters
		P_eta_map,
		P_n_hel,
		P_q_start,
		P_p_run,
		P_v_wind_max,
		P_hel_stow_deploy,
		P_interp_beta,
		P_interp_nug,

		//Inputs
		I_v_wind,
		I_field_control,
		I_theta,
		I_phi,

		//Outputs
		O_pparasi,
		O_eta_field,

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_type221B_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_MATRIX, P_eta_map,			"eta_map",			"Field efficiency matrix",             			        "-",        "3 columns (azimuth, zenith, field efficiency)", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_hel,            "n_hel",            "Number of heliostats in the field",					"-",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_q_start,			"q_start",			"Electric work for starting up one heliostat",			"kWe-hr",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_p_run,			"p_run",			"Electric power for tracking one heliostat",			"kWe",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_v_wind_max,		"v_wind_max",		"Maximum tolerable wind speed",							"m/s",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_hel_stow_deploy,	"hel_stow_deploy",	"Heliostat field stow/deploy solar elevation angle",	"deg",		"", "", "10."},
	{TCS_PARAM, TCS_NUMBER, P_interp_beta,		"interp_beta",		"Interpolation beta coef",								"-",		"", "", "1.99"},
	{TCS_PARAM, TCS_NUMBER, P_interp_nug,		"interp_nug",		"Interpolation nugget",									"-",		"", "", "0.0"},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_v_wind,			"vwind",			"Wind velocity",										"m/s",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_field_control,	"field_control",	"Field defocus control",								"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_theta,			"theta",			"Solar zenith angle",									"deg",	    "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_phi,				"phi",				"Solar azimuth angle: 0 due north, clockwise to +360",	"deg",		"", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_pparasi,			"pparasi",			"Parasitic tracking/startup power",						"MWe",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_eta_field,		"eta_field",		"Total field efficiency",								"",			"", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	}
};
	
	
class sam_mw_pt_type221B : public tcstypeinterface
{
private:
	// Class Instances
	GaussMarkov *field_efficiency_table;

	//Parameters
	double n_hel;
	double q_start;
	double p_run;
	double v_wind_max;	
	double hel_stow_deploy;
	double interp_beta;
	double interp_nug;
			
	//Stored Variables
	double eta_prev;
	double v_wind_prev;
	double az_scale;
	double zen_scale;
	double eff_scale;

public:
	sam_mw_pt_type221B( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		n_hel = std::numeric_limits<double>::quiet_NaN();
		q_start = std::numeric_limits<double>::quiet_NaN();
		p_run = std::numeric_limits<double>::quiet_NaN();
		v_wind_max = std::numeric_limits<double>::quiet_NaN();
		hel_stow_deploy = std::numeric_limits<double>::quiet_NaN();
		interp_beta = std::numeric_limits<double>::quiet_NaN();
		interp_nug = std::numeric_limits<double>::quiet_NaN();

		eta_prev = std::numeric_limits<double>::quiet_NaN();
		v_wind_prev = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_mw_pt_type221B()
	{
		delete field_efficiency_table;
	}

	virtual int init()
	{
		// Read in solar field efficiency file: 3 rows (0: azimuth, 1: zenith, 2: solar field efficiency)
		// TFF, Sept 17 2013: Note that the script DSG_PT_Defaults.lk loads this array with [ 0: zenith, 1: azimuth, 2: solar field efficiency]

		MatDoub sunpos;
		vector<double> effs;
		
		int nrows, ncols;
		double *p_map = value( P_eta_map, &nrows, &ncols);
		
		if(ncols != 3){
			message("The heliostat field efficiency file is not formatted correctly. Type 221B expects 3 columns"
				" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);
			return -1;
		}
		
		//read the data from the array into the local storage arrays
		az_scale = 360.;
		zen_scale = 90.;
		eff_scale = 0.7;

		sunpos.resize(nrows, VectDoub(2));
		effs.resize(nrows);
		for(int i=0; i<nrows; i++){
			sunpos.at(i).at(0) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 0 ) / az_scale;
			sunpos.at(i).at(1) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 1 ) / zen_scale;
			effs.at(i) = TCS_MATRIX_INDEX( var( P_eta_map ), i, 2 ) / eff_scale;
		}
		
		//collect nug and beta
		interp_beta = value(P_interp_beta);
		interp_nug = value(P_interp_nug);

		//Create the field efficiency table
		Powvargram vgram(sunpos, effs, interp_beta, interp_nug);
		field_efficiency_table = new GaussMarkov(sunpos, effs, vgram);

		//test how well the fit matches the data
		double err_fit = 0.;
		for(int i=0; i<nrows; i++){
			double dz = field_efficiency_table->interp( sunpos.at(i) ) - effs.at(i);
			err_fit += dz * dz;
		}
		err_fit = sqrt(err_fit);
		if( err_fit > 0.01 )
			message("The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);

		// **************************************
		// Read in parameters
		n_hel = value( P_n_hel );					// [-] Number of heliostats
		q_start = value( P_q_start ) * 3600.0;		// [kJ] convert from kWe-hr
		p_run = value( P_p_run ) * 3600.0;			// [kJ/hr] convert from kWe
		v_wind_max = value( P_v_wind_max );			// [m/s] Wind speed at which heliostats are stowed
		hel_stow_deploy = value( P_hel_stow_deploy ); // [deg] Heliostat field stow/deploy solar elevation angle

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
		double theta = value( I_theta );	// solar zenith angle 
		if( theta >= 90.0 )
			field_control = 0.0;		// No tracking before sunrise of after sunset
		double phi = value( I_phi );	
		

      
		// Parasitics for startup or shutdown
		double pparasi = 0.0; 
		
		// If starting up or shutting down, calculate parasitics
		if( (field_control > 1.e-4 && eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= v_wind_max ) ||		// OR Shutdown by high wind speed
		(eta_prev > 1.e-4 && v_wind_prev >= v_wind_max && v_wind < v_wind_max)  )	// OR Startup after high wind speed
			pparasi = n_hel * q_start / (step/3600.0);			// kJ/hr 
     
		// Parasitics for tracking      
		if( v_wind < v_wind_max && v_wind_prev < v_wind_max )
				pparasi += n_hel * p_run * field_control;	// kJ/hr

		double eta_field = 0.;

		if( theta > (89.9 - hel_stow_deploy) || v_wind > v_wind_max ){
			eta_field = 1.e-6;
		}
		else{
				
			// Use current solar position to interpolate field efficiency table and fied solar field efficiency
			vector<double> sunpos;
			sunpos.push_back(phi/az_scale);
			sunpos.push_back(theta/zen_scale);

			eta_field = field_efficiency_table->interp( sunpos ) * eff_scale;
			eta_field = min( max ( eta_field, 0.0 ), 1.0 ) * field_control;		// Ensure physical behavior 
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

};

TCS_IMPLEMENT_TYPE( sam_mw_pt_type221B, "Heliostat field with irregular efficiency data", "Mike Wagner", 1, sam_mw_pt_type221B_variables, NULL, 1 )

