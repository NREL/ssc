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

#ifndef __lib_windwake
#define __lib_windwake

#include <vector>
#include "lib_util.h"

class windTurbine
{
private:
	double rotorDiameter, hubHeight, measurementHeight, shearExponent;
	std::vector<double> powerCurveWS,			// windspeed: x-axis on turbine power curve
						powerCurveKW,			// power output: y-axis
						densityCorrectedWS,
						powerCurveRPM;
	unsigned int powerCurveArrayLength;
	double cutInSpeed;
	double lossesAbsolute, lossesPercent;
public:
	std::string errDetails;

	windTurbine(){ 
		shearExponent = -1;
		measurementHeight = -1;
		hubHeight = -1;
		rotorDiameter = -1;
		lossesAbsolute = -1;
		lossesPercent = -1;
	}
	void setCharacteristics(double h, double d){ hubHeight = h; rotorDiameter = d; }
	void setShearExponent(double s) { shearExponent = s; }
	void setWeatherInfo(double mh){ measurementHeight = mh; }
	void setLosses(double a, double p){ lossesAbsolute = a; lossesPercent = p; }
	bool setPowerCurve(std::vector<double> windSpeeds, std::vector<double> powerOutput);
	
	double tipSpeedRatio(double windSpeed);

	bool isInitialized(){
		if (shearExponent > 0 && measurementHeight > 0 && hubHeight > 0 && rotorDiameter > 0 && lossesAbsolute >= 0 && lossesPercent >= 0){
			if (powerCurveArrayLength > 0) return true;
		}
		return false;
	}
	void turbinePower(double windVelocityAtDataHeight, double airDensity, double *turbineOutput, double *thrustCoefficient);
};

class wake_model
{
protected:
	unsigned int nTurbines;
public:
	wake_model(){}
	std::string errDetails;
	virtual int test(int a){ return a + 10; }
	virtual void wakeCalculations(
		const double airDensity, const double distanceDownwind[], const double distanceCrosswind[],
		double power[], double thrust[], double windSpeed[], double turbulenceIntensity[]) = 0;
};

/**
* simpleWakeModel uses Pat Quinlan's model: calculate the wind speed deficit factor to estimate the reduction in wind speed
* at a downwind turbine due to the wake of an upwind turbine. The model returns the modified wind speed for each turbine.
*/

class simpleWakeModel : public wake_model{
private:	
	double velDeltaPQ(double radiiCrosswind, double axialDistInRadii, double thrustCoeff, double *newTurbulenceIntensity);

public:
	simpleWakeModel(){ nTurbines = 0; }
	simpleWakeModel(int numberOfTurbinesInFarm){ nTurbines = numberOfTurbinesInFarm; }

	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
	);
};

/**
* The Park / WAsP model assumes the turbine wake width expands linearly downstream, dependent on wake-decay
* constant k = 0.07, empirically determined. The wind speed deficit of a downstream turbine depends on how
* much its swept area overlaps with the upstream wake. 
*/

class parkWakeModel : public wake_model{
private:
	double rotorDiameter;
	double wakeDecayCoefficient = 0.07, 
		   minThrustCoeff = 0.02;
	double delta_V_Park(double dVelFreeStream, double dVelUpwind, double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff);
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);

public:
	parkWakeModel(){ nTurbines = 0; }
	parkWakeModel(int numberOfTurbinesInFarm){ nTurbines = numberOfTurbinesInFarm; }

	void setRotorDiameter(double d){ rotorDiameter = d; }
	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
	);
};


class eddyViscosityWakeModel : public wake_model{
private:
	windTurbine* wTurbine;
	double rotorDiameter, turbulenceCoeff;
	double axialResolution, minThrustCoeff, nBlades;
	int MIN_DIAM_EV;
	util::matrix_t<double> matEVWakeDeficits;	// wind velocity deficit behind each turbine, indexed by axial distance downwind
	util::matrix_t<double> matEVWakeWidths;		// width of wake (in diameters) for each turbine, indexed by axial distance downwind

	struct VMLN
	{
		VMLN(){}
		virtual ~VMLN(){}

		double m;
		double Ro;
		double Xh;
		double Xn;
		double Rh;
		double Rn;
		double Xf;
		double Rf;
		double dUc_Uinf_Xn;
		double diam;
	};

	/// get the velocity deficit at an axial distance behind a given upwind turbine using the wake deficit matrix
	double getVelocityDeficit(int upwindTurbine, double axialDistanceInDiameters);

	/// get the wake width at a radial distance from the centerline, using the given upwind turbine's wake widths matrix
	double getWakeWidth(int upwindTurbine, double axialDistanceInDiameters);

	/// get deficit by modeling wake beyond near wake region as axisymmetric profile with Gaussian cross-section
	double wakeDeficit(int upwindTurbine, double distCrosswind, double distDownwind);

	/// Use the Pat Quinlan method to get added TI
	double addedTurbulenceIntensity(double Ct, double deltaX);
	
	double totalTurbulenceIntensity(double ambientTI, double additionalTI, double Uo, double Uw, double partial);

	/// Using Ii, ambient turbulence intensity, and thrust coeff, calculates the length of the near wake region
	void nearWakeRegionLength(double U, double Ii, double Ct, double airDensity, VMLN& vmln);

	/// Returns the intersection area between the downwind turbine swept area and the wake
	double simpleIntersect(double distToCenter, double radiusTurbine, double radiusWake);

public:
	eddyViscosityWakeModel(){ nTurbines = 0; }
	eddyViscosityWakeModel(int numberOfTurbinesInFarm){ 
		nTurbines = numberOfTurbinesInFarm; 
		axialResolution = 0.5;
		minThrustCoeff = 0.02;
		nBlades = 3;
		MIN_DIAM_EV = 2;
	}

	void setRotorDiameter(double d){ rotorDiameter = d; }
	void setTurbulenceCoeff(double t){ turbulenceCoeff = t; }
	void setTurbine(windTurbine* wt){ wTurbine = wt; }	// required

	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
		);
};

#endif