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

#ifndef __lib_windwatts_h
#define __lib_windwatts_h

#include <memory>
#include <vector>
#include "lib_util.h"
#include "lib_windwakemodel.h"

enum {PAT_QUINLAN_WAKE_MODEL, PARK_WAKE_MODEL, SIMPLE_EDDY_VISCOSITY_WAKE_MODEL, OLD_PQ};

static inline double max_of(double a, double b)
{	return (a > b) ? a : b;	}

static inline double min_of(double a, double b)
{	return (a < b) ? a : b; }

class wind_power_calculator
{
public:
	wind_power_calculator() {
		m_dShearExponent = 1.0/7.0;
		m_iNumberOfTurbinesInFarm=m_iLengthOfTurbinePowerCurveArray=m_iControlMode=0;
		m_dMeasurementHeight=m_dHubHeight=m_dRotorDiameter=m_dCutInSpeed=m_dRatedSpeed=m_dRatedPower=m_dLossesAbsolute=m_dLossesPercent=0;
		m_sErrDetails="";

		// private member vars, acting like constants right now (no access to change them)
		m_dAxialResolution = 0.5; // in rotor diameters, default in openWind=0.5
		m_dRadialResultion = 0.2; // in rotor diameters, default in openWind=0.2
		m_dMaxRotorDiameters = 50; // in rotor diameters, default in openWind=50
		m_dMinThrustCoeff = 0.02;
		m_dMinDeficit=0.0002;
		m_bFilter = true;
		m_iNumberOfBlades = 3;
	}
	
	static const int MAX_WIND_TURBINES = 300; // Max turbines in the farm
	static const int MIN_DIAM_EV = 2; // Minimum number of rotor diameters between turbines for EV wake modeling to work
	static const int EV_SCALE = 1; // Uo or 1.0 depending on how you read Ainslie 1988

	double m_dShearExponent;		// also referred to as Alpha
	double m_dTurbulenceIntensity;	// also referred to as Sigma
	size_t m_iNumberOfTurbinesInFarm;
	size_t m_iLengthOfTurbinePowerCurveArray;
	double m_dMeasurementHeight;	// height at which the reource data was measured
	double m_dHubHeight;			// hub height of turbines (m)
	double m_dRotorDiameter;		// rotor diameter (m)
	int m_iControlMode;				// control mode 0=pitch, 1=variable, 2=simple/stall
	int m_iWakeModelChoice;			// 0=Original Model (based on Pat Quinlan's thesis), 1=Park Model, 2=Eddy viscosity model
	double m_dCutInSpeed;			// wind speed Cut in (m/s)
	double m_dRatedSpeed;			// rated wind speed
	double m_dRatedPower;			// rated power (kw)
	double m_dLossesAbsolute;		// constant loss
	double m_dLossesPercent;		// loss as percent
	double m_dWakeDecayCoefficient; // wake decay coefficient (k)

	std::vector<double> m_adPowerCurveWS, m_adPowerCurveKW, m_adPowerCurveRPM, m_adXCoords, m_adYCoords, m_adDensityCorrectedWS;

	size_t GetMaxTurbines() {return MAX_WIND_TURBINES;}
	bool InitializeModel(std::shared_ptr<wake_model>selectedWakeModel); // if necessary, allocate memory in util::matrix arrays
	std::string GetWakeModelShortName();
	std::string GetWakeModelName();
	std::string GetErrorDetails() { return m_sErrDetails; }

	int wind_power(
		// INPUTS
			double Vel_T,    // wind velocity m/s
			double Theta_T,  // wind direction 0-360, 0=N
			double BarPAtm,  // barometric pressure (Atm)
			double TdryC,    // dry bulb temp ('C)
			
		// OUTPUTS
			double *FarmP,    // total farm power output
			double aPower[],  // calculated power of each WT
			double aThrust[], // thrust calculation at each WT
			double aEff[],    // downwind efficiency of each WT
			double aWind[],   // wind speed at each WT
			double aTurbul[], // turbulence coeff at each WT
			double aDistDown[], // distance down wind
			double aDistCross[] // distance cross wind
		);

	double turbine_output_using_weibull(
		double weibull_k, 
		double max_cp, 
		double avg_speed, 
		double ref_height,
		double energy_turbine[]
	);

	double tip_speed_ratio(double dWindSpeed);
	void turbine_power(double fWindVelocityAtDataHeight, double fAirDensity, double *fTurbineOutput, double *fThrustCoefficient);

private:
	std::shared_ptr<wake_model> wakeModel;
	std::string m_sErrDetails;
	util::matrix_t<double> matEVWakeDeficits; // wind velocity deficit behind each turbine, indexed by axial distance downwind
	util::matrix_t<double> matEVWakeWidths; // width of wake (in diameters) for each turbine, indexed by axial distance downwind

	// private variables acting like constants
	double m_dAxialResolution;	// resolution for EV wake calculations along axial direction
	double m_dRadialResultion;	// resolution for EV wake calculations along radial direction
	double m_dMaxRotorDiameters; // how far down wind will EV calculations go
	double m_dMinThrustCoeff;	// limit lower bound of thrust coeff (Ct) for calculations
	bool m_bFilter;
	double m_dMinDeficit; // below this value, the simple EV model stops calculating deficits, so they are let at zero
	int m_iNumberOfBlades;

	struct VMLN
	{
		VMLN(){}
		virtual ~VMLN(){}
	
		double m;
		double Ro;
		double Xh;				
		double Xn;			// length of the near wake region, in terms of rotor radius and Ct
		double Rh;
		double Rn;
		double Xf;
		double Rf;
		double dUc_Uinf_Xn;
		double diam;
	};

	void wake_calculations_pat_quinlan_mod(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);

	void wake_calculations_Park(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[]				// wind speed at each WT
	);

	 // Implements a simplified Eddy-Viscosity model as per "Simplified Soultion To The Eddy Viscosity Wake Model" - 2009 by Dr Mike Anderson of RES
	bool wake_calculations_EddyViscosity_Simple(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);

	// original Pat Quinlan model, before other wake models added
	void wake_calculations_pat_quinlan_old(
		/*INPUTS*/
		double fAir_density,
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
	);


	double get_EV_wake_width(int iUpwindTurbine, double dAxialDistanceInDiameters);
	double get_EV_velocity_deficit(int iUpwindTurbine, double dAxialDistanceInDiameters);
	double wake_deficit_EV(int iUpwindTurbine, double dDistCrossWind, double dDistDownWind);
	double calc_EV_added_turbulence_intensity(double IatUpstreamTurbine, double Ct,double deltaX, VMLN& vmln);
	double calc_EV_total_turbulence_intensity(double ambientTI, double additionalTI, double Uo, double Uw, double partial);
	bool fill_turbine_wake_arrays_for_EV(int iTurbineNumber, double dAmbientVelocity, double dVelocityAtTurbine, double dPower, double dThrustCoeff, double dTurbulenceIntensity, double maxX);
	/// Using Ii, ambient turbulence intensity, and thrust coeff, calculates the length of the near wake region
	void calc_EV_vm_for_turbine(double U, double Ii, double Ct, double airDensity, VMLN& vmln);
	
	
	double vel_delta_PQ( double fRadiiCrosswind, double fRadiiDownwind, double fThrustCoeff, double *fNewTurbulenceIntensity);
	double delta_V_Park( double dVelFreeStream, double dVelUpwind, double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff);
	void coordtrans( double fMetersNorth, double fMetersEast, double fWind_dir_degrees, double *fMetersDownWind, double *fMetersCrosswind);
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);
	double simple_intersect(double dist_center_to_center, double rad1, double rad2);
	double gammaln(double x);

};

#endif
