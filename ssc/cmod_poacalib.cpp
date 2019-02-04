/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "core.h"
#include "lib_irradproc.h"
#include "lib_util.h"

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_poacalib[] = 
{	
/*   VARTYPE           DATATYPE         NAME                 LABEL                     UNITS               META                 GROUP            REQUIRED_IF    CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "latitude",          "Latitude",              "decimal degrees",  "N= positive",       "POA Calibrate", "*",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "longitude",         "Longitude",             "decimal degrees",  "E= positive",       "POA Calibrate", "*",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "time_zone",         "Time Zone",             "",                 "-7= Denver",        "POA Calibrate", "*",           "MIN=-12,MAX=12",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "array_tilt",        "Array tilt",            "degrees",          "0-90",              "POA Calibrate", "*",           "MIN=0,MAX=90",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "array_az",          "Array Azimuth",         "degrees",          "0=N, 90=E, 180=S",  "POA Calibrate", "*",           "MIN=0,MAX=360",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "year",              "Year",                  "",                 "",                  "POA Calibrate", "*",           "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",            "Albedo",                "",                 "",                  "POA Calibrate", "*",           "MIN=0,MAX=1",                   "" },

	{ SSC_INPUT,        SSC_ARRAY,       "poa",               "Plane of Array",        "W/m^2",            "",                  "POA Calibrate", "*",           "LENGTH=8760",                   "" },

	{ SSC_INOUT,        SSC_ARRAY,       "beam",              "Beam Irradiation",      "W/m^2",            "",                  "POA Calibrate", "*",           "LENGTH=8760",                   "" },
	{ SSC_INOUT,        SSC_ARRAY,       "diffuse",           "Diffuse Irradiation",   "W/m^2",            "",                  "POA Calibrate", "*",           "LENGTH=8760",                   "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "pcalc",            "Calculated POA",        "W/m^2",            "",                  "POA Calibrate", "*",           "",                   "" },


var_info_invalid };

class cm_poacalib : public compute_module
{
private:
public:
	cm_poacalib()
	{
		add_var_info( _cm_vtab_poacalib );
	}

	void exec( ) throw( general_error )
	{

		/* Changes input Beam and Diffuse irradiation so that they yield a POA equal to input POA using the Perez transposition model, while maintaining the input ratio between beam & diffuse.
		Function assumes that input POA has been error-checked (ie: no irradiance values at night, no negative values, no unreal values). Also assumes that input beam and diffuse are always >= 0 
		Program is currently set up for FIXED TILT ONLY*/

		// assign constants
		double lat = as_double("latitude");
		double lon = as_double("longitude");
		double tilt = as_double("array_tilt"); // in degrees
		double az = as_double("array_az"); // in degrees
		double timezone = as_double("time_zone");
		int year = as_integer("year");
		double alb = as_double("albedo");

		// pointers to irradiance arrays
		size_t num_steps;
		ssc_number_t *poa = as_array( "poa", &num_steps );
		ssc_number_t *beam = as_array( "beam", &num_steps );
		ssc_number_t *diffuse = as_array( "diffuse", &num_steps );
		ssc_number_t *pcalc = allocate( "pcalc", num_steps );

		// loop through 8760 timesteps
		int idx = 0;
		for (int m=1;m<=12;m++ ) //index across months
		{
			for( size_t d=1;d<=util::nday[m-1];d++ ) //index across days of month
			{
				for( int h=0;h<24;h++ ) //index across hours
				{
					// assign current timestep irradiance values to variables
					double P = poa[idx];
					double D = diffuse[idx];
					double B = beam[idx];

					// check for nighttime
					if (P <= 0 )
					{
						beam[idx] = 0;
						diffuse[idx] = 0;
						pcalc[idx] = 0;
						idx++;
						continue;
					}

					// call irradiance class for needed variables and assign variables
					irrad x;
					x.set_location(lat, lon, timezone);
					x.set_time( year, m, (int)d, h, 30, 1.0  );
					x.set_surface(0, tilt, az, 0, 0, 0);
					x.set_sky_model(2, alb);
					x.set_beam_diffuse(B, D);
					double solaz, zen;
					x.calc();
					x.get_sun(&solaz, &zen, 0, 0, 0, 0, 0, 0, 0, 0);
					solaz = solaz*DTOR;
					zen = zen*DTOR;
					double inc;
					x.get_angles(&inc, 0, 0, 0, 0);
					inc = inc*DTOR;

					// define beam to diffuse ratio (remains unchanged for this timestep)
					double R = B/D;
					// Inc angle greater than 90 degrees OR measured P but no measured D OR measured P less than 1 -> assume all diffuse
					if( inc >= DTOR*90 || (P>0 && D<=0) || P<1 )
						R = 0;

					// compute calculated POA using Perez method starting with input B & D
					double poa[3] = {0,0,0};
					double diffc[3] = {0,0,0};
					perez(0, B, D, alb, inc, DTOR*tilt, zen, poa, diffc);
					double Pcalc = poa[0] + poa[1] + poa[2];

					// for high zenith angles, save original values to check for unreasonable values after iteration
					double B_o = B;
					double D_o = D;
					bool flag = 0; // flag for unreasonable values at high zenith angles
										
					// iterate for Perez POA to match input POA
					int counter = 0; // counter to prevent an infinite loop
					while( fabs(Pcalc - P) > 0.5 && counter < 5000 )
					{
						// incrementally increase or reduce D based on difference between P calculated and P measured
						double incr = fabs(Pcalc - P)*0.01;
						if( Pcalc > P )
							D = D - incr;
						else
							D = D + incr;

						// increment B according to ratio if not flagged by high zenith angle, otherwise leave B alone
						if ( !flag )
							B = D*R;
						else
							B = B_o;

						// compute new calculated P
						perez(0, B, D, alb, inc, DTOR*tilt, zen, poa, diffc);
						Pcalc = poa[0] + poa[1] + poa[2];

						// check that high zenith Beam isn't getting ridiculous
						if ( zen > DTOR*85 )
						{
							if ( (B-B_o) > 100 ) // if Beam getting too far away from input beam at high zenith
							{
								flag = 1; // turn on flag so that beam does not get incremented
								B = B_o; // reset beam and diffuse
								D = D_o;
								perez(0, B, D, alb, inc, DTOR*tilt, zen, poa, diffc);
								Pcalc = poa[0] + poa[1] + poa[2]; // reset Pcalc
								counter = 0; // reset counter
							}
						}
						counter++;
					}
					
					// assign error value if didn't converge in 5000 steps
					if( counter == 5000 || B < 0 || D < 0 )
						B = D = -999;

					// assign calibrated beam and diffuse to outputs
					beam[idx] = (ssc_number_t) B;
					diffuse[idx] = (ssc_number_t) D;
					pcalc[idx] = (ssc_number_t) Pcalc;

					idx++;
				}
			}
		}	

	}
};

DEFINE_MODULE_ENTRY( poacalib, "Calibrates beam and diffuse to give POA input", 1 )
