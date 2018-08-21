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

/**
* \file
* \brief File containing calculations for front-side and rear-side irradiance of tracked-tilted surfaces.
*/

/**
* \brief Function calculates the sun position given the local standard time and location.
*   This function is based on a paper by Michalsky published in Solar Energy
*	Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
*	time and location passed to the function based on the Astronomical
*	Almanac's Algorithm for the period 1950-2050. For data averaged over an
*	interval, the appropriate time passed is the midpoint of the interval.
*	(Example: For hourly data averaged from 10 to 11, the time passed to the
*	function should be 10 hours and 30 minutes). The exception is when the time
*	interval includes a sunrise or sunset. For these intervals, the appropriate
*	time should be the midpoint of the portion of the interval when the sun is
*	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
*	sunrise time of 7:30, the time passed to the function should be 7 hours and
*	and 45 minutes).
*
* \param year (e.g. 1986)
* \param month (1-12)
* \param day day of month
* \param hour hour of day local standard time (1-24)
* \param minute minutes pas the hour, local standard time (1-24)
* \param lat latitude in degrees, north positive
* \param lng longitude in degrees, east positive
* \param tz time zone, west longitudes negative
* \param sunn array of elements to return sun parameters to calling function
* \param sunn[0] sun azimuth in radians, measured east from north, 0 to 2*pi
* \param sunn[1] sun zenith in radians, 0 to pi
* \param sunn[2] sun elevation in radians, -pi/2 to pi/2
* \param sunn[3] sun declination in radians
* \param sunn[4] sunrise in local standard time (hrs), not corrected for refraction
* \param sunn[5] sunset in local standard time (hrs), not corrected for refraction
* \param sunn[6] eccentricity correction factor
* \param sunn[7] true solar time (hrs)
* \param sunn[8] extraterrestrial solar irradiance on horizontal at particular time (W/m2)
* \return Array of sun angles passed by reference
*/
void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9]);

/**
* \brief Function calculates the incident angle of direct beam radiation to a surface.
* The calculation is done for a given sun position, latitude, and surface orientation. 
* The modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
*
* \param mode 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking, 3 for azimuth-axis tracking, 4 for timeseries tilt tracking (in "set surface" function, this is set as mode 0)
* \param tilt tilt angle of surface from horizontal in degrees (mode 0), or tilt angle of tracker axis from horizontal in degrees (mode 1), MUST BE FROM 0 to 90 degrees.
* \param sazm surface azimuth in degrees of collector (mode 0), 
				or surface azimuth of tracker axis (mode 1) with axis azimuth directed from raised to lowered end of axis if axis tilted.
* \param rlim plus or minus rotation in degrees permitted by physical constraints of tracker, range is 0 to 180 degrees.
* \param zen sun zenith in radians, MUST BE LESS THAN PI/2
* \param azm sun azimuth in radians, measured east from north
* \param en_backtrack enable backtracking, using Ground coverage ratio ( below )
* \param gcr  ground coverage ratio ( used for backtracking )
* \param angle array of elements to return angles to calling function
* \param angle[0] incident angle in radians
* \param angle[1] tilt angle of surface from horizontal in radians
* \param angle[2] surface azimuth in radians, measured east from north
* \param angle[3] tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
* \param angle[4] backtracking difference (rot - ideal_rot) will be zero except in case of backtracking for 1 axis tracking
* \return Array of surface angles passed by reference
*/
void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5]);


/**
* \brief Defines the Perez function for calculating values of diffuse + direct 
* solar radiation + ground reflected radiation for a tilted surface and returns the total plane-of-array irradiance(poa),
* see also isotropic(), hdkr().
*
* Function does not check all input for valid entries; consequently, this should be
* done before calling the function.  (Reference: Perez et al, Solar Energy Vol. 44, No.5, pp.271-289,1990.) 
*
* \param hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param dn direct normal radiation (W/m2)
* \param df diffuse horizontal radiation (W/m2)
* \param alb surface albedo (decimal fraction)
* \param inc incident angle of direct beam radiation to surface in radians
* \param tilt surface tilt angle from horizontal in radians
* \param zen sun zenith angle in radians
* \param poa calculated plane-of-array irradiances (W/m2)
* \param poa[0] incident beam
* \param poa[1] incident sky diffuse
* \param poa[2] incident ground diffuse
* \param diffc diffuse components, if an array is provided
* \param diffc[0] isotropic diffuse
* \param diffc[1] circumsolar diffuse
* \param diffc[2] horizon brightening
* \return Array of poa irradiances and diffuse irradiance components passed by reference
*/
void perez( double hextra, double dn,double df,double alb,double inc,double tilt,double zen, double poa[3], double diffc[3] /* can be NULL */ );

/**
* \brief Defines isotropic sky model for diffuse irradiance on a tilted surface, see also perez(), hdkr().
*
* \param hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param dn direct normal radiation (W/m2)
* \param df diffuse horizontal radiation (W/m2)
* \param alb surface albedo (decimal fraction)
* \param inc incident angle of direct beam radiation to surface in radians
* \param tilt surface tilt angle from horizontal in radians
* \param zen sun zenith angle in radians
* \param poa calculated plane-of-array irradiances (W/m2)
* \param poa[0] incident beam
* \param poa[1] incident sky diffuse
* \param poa[2] incident ground diffuse
* \param diffc diffuse components, if an array is provided
* \param diffc[0] isotropic diffuse
* \param diffc[1] circumsolar diffuse
* \param diffc[2] horizon brightening
* \return Array of poa irradiances and diffuse irradiance components passed by reference
*/
void isotropic( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be NULL */ );

/**
* \brief Defines Hay, Davies, Klutcher, Reindl model for diffuse irradiance on a tilted surface, see also perez(), isotropic().
*
* \param hextra extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
* \param dn direct normal radiation (W/m2)
* \param df diffuse horizontal radiation (W/m2)
* \param alb surface albedo (decimal fraction)
* \param inc incident angle of direct beam radiation to surface in radians
* \param tilt surface tilt angle from horizontal in radians
* \param zen sun zenith angle in radians
* \param poa calculated plane-of-array irradiances (W/m2)
* \param poa[0] incident beam
* \param poa[1] incident sky diffuse
* \param poa[2] incident ground diffuse
* \param diffc diffuse components, if an array is provided
* \param diffc[0] isotropic diffuse
* \param diffc[1] circumsolar diffuse
* \param diffc[2] horizon brightening
* \return Array of poa irradiances and diffuse irradiance components passed by reference
*/
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
