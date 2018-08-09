/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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

#ifndef __irradproc_h
#define __irradproc_h

#include "lib_pv_io_manager.h"

/* aug2011 - apd
	solar position and radiation processing split out from pvwatts.
	added isotropic sky model and hdkr model for diffuse on a tilted surface
	*/

void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5]);
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );
void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

// Create functions for POA decomposition
void poaDecomp( double wfPOA, double angle[], double sun[], double alb, poaDecompReq* pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]);
double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn);
void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double &dn);

class irrad
{
private:

	poaDecompReq * poaAll;
	Irradiance_IO * irradiance;
	Subarray_IO * subarray;

	// Time inputs
	int year, month, day, hour;
	double minute, delt;

	// Position inputs
	double latitudeDegrees;
	double longitudeDegrees;
	double timezone;

	// Model settings
	int radiationMode;
	int skyModel;
	int trackingMode;
	bool enableBacktrack;

	// Input Front-Side Irradiation components 
	double globalHorizontal;
	double directNormal;
	double diffuseHorizontal;
	double weatherFilePOA;
	double albedo;

	// Calculated Front-Side Irradiation components
	double calculatedDirectNormal;
	double calculatedDiffuseHorizontal;

	// Subarray properties
	double tiltDegrees;
	double surfaceAzimuthDegrees;
	double rotationLimitDegrees;
	double groundCoverageRatio;

	// Outputs
	double sunAnglesRadians[9];
	double surfaceAnglesRadians[5];
	double planeOfArrayIrradianceFront[3];
	double planeOfArrayIrradianceRear[3];
	double diffuseIrradianceFront[3];
	double diffuseIrradianceRear[3];
	int timeStepSunPosition[3];
	double planeOfArrayIrradianceRearAverage;

public:

	irrad();
	irrad(Irradiance_IO * , Subarray_IO *);

	void setup();
	int check();
	
	// if delt_hr is less than zero, do not interpolate sunrise and sunset hours
#define IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET (-1.0)
	void set_time( int year, int month, int day, int hour, double minute, double delt_hr );
	void set_location( double lat, double lon, double tz);
	//skymodel: 0 is isotropic, 1 is hdkr, 2 is perez
	void set_sky_model( int skymodel, double albedo );
	void set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr );
	void set_beam_diffuse( double beam, double diffuse );
	void set_global_beam( double global, double beam );
	void set_global_diffuse(double global, double diffuse);
	void set_poa_reference( double poa, poaDecompReq* );
	void set_poa_pyranometer( double poa, poaDecompReq* );

	/// Function to overwrite internally calculated sun position values, primarily to enable testing against other libraries using different sun position calculations
	void set_sun_component(size_t index, double value);

	int calc();
	int calc_rear_side(double transmissionFactor, double bifaciality, double groundClearanceHeight, double slopeLength);
	
	void get_sun( double *solazi,
		double *solzen,
		double *solelv,
		double *soldec,
		double *sunrise,
		double *sunset,
		int *sunup,
		double *eccfac,
		double *tst,
		double *hextra );
	double get_sun_component(size_t i) { return sunAnglesRadians[i]; }
	void get_angles( double *aoi,
		double *surftilt,
		double *surfazi,
		double *axisrot,
		double *btdiff );
	void get_poa( double *beam, double *skydiff, double *gnddiff,
		double *isotrop, double *circum, double *horizon );
	double get_poa_rear();
	void get_irrad (double *ghi, double *dni, double *dhi);
	double get_sunpos_calc_hour();
	double getAlbedo();

	void getSkyConfigurationFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> & rearSkyConfigFactors, std::vector<double> & frontSkyConfigFactors);
	void getGroundShadeFactors(double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, double solarAzimuthRadians, double solarElevationRadians, std::vector<int> & rearGroundFactors, std::vector<int> & frontGroundFactors, double & maxShadow, double & pvBackShadeFraction, double & pvFrontShadeFraction);
	void getGroundGHI(double transmissionFactor, std::vector<double> rearSkyConfigFactors, std::vector<double> frontSkyConfigFactors, std::vector<int> rearGroundShadeFactors, std::vector<int> frontGroundShadeFactors, std::vector<double> & rearGroundGHI, std::vector<double> & frontGroundGHI);
	void getBackSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> rearGroundGHI, std::vector<double> frontGroundGHI, std::vector<double> frontReflected, std::vector<double> & rearIrradiance, double & rearAverageIrradiance);
	void getFrontSurfaceIrradiances(double pvBackShadeFraction, double rowToRow, double verticalHeight, double clearanceGround, double distanceBetweenRows, double horizontalLength, std::vector<double> frontGroundGHI, std::vector<double> & frontIrradiance, double & frontAverageIrradiance, std::vector<double> & frontReflected);
};




double shade_fraction_1x( double solazi, double solzen, 
						 double axis_tilt, double axis_azimuth, 
						 double gcr, double rotation );

double backtrack( double solazi, double solzen, 
				 double axis_tilt, double axis_azimuth, 
				 double rotlim, double gcr, double rotation_ideal);

#endif
