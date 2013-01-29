#ifndef __lib_windwatts_h
#define __lib_windwatts_h

#include <vector>

class wind_power_calculator
{
public:
	wind_power_calculator() {
		m_dShearExponent=1.0/7.0;
		m_iNumberOfTurbinesInFarm=m_iLengthOfTurbinePowerCurveArray=m_iControlMode=0;
		m_dMeasurementHeight=m_dHubHeight=m_dRotorDiameter=m_dCutInSpeed=m_dRatedSpeed=m_dRatedPower=m_dLossesAbsolute=m_dLossesPercent=0;
	}
	
	static const int MAX_WIND_TURBINES = 300;


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

	std::vector<double> m_adPowerCurveWS, m_adPowerCurveKW, m_adXCoords, m_adYCoords;

	int GetMaxTurbines() {return MAX_WIND_TURBINES;}

	int wind_power(
		// INPUTS
			double Vel_T,    // wind velocity m/s
			double Theta_T,  // wind direction 0-360, 0=N
			double BarPAtm,  // barometric pressure (Atm)
			double TdryC,    // dry bulb temp ('C)
			
		// OUTPUTS
			double *FarmP,   // total farm power output
			double Power[],  // calculated power of each WT
			double Thrust[], // thrust calculation at each WT
			double Eff[],    // downwind efficiency of each WT
			double Wind[],   // wind speed at each WT
			double Turbul[]  // turbulence coeff at each WT
		);

	double turbine_output_using_weibull(
		double weibull_k, 
		double max_cp, 
		double resource_class, 
		double hub_efficiency[]
	);

private:
	void wake_calculations_pat_quinlan(
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
		double dWakeDecayK,					// wake decay coefficient (k)
		double aDistanceDownwind[],			// downwind coordinate of each WT
		double aDistanceCrosswind[],		// crosswind coordinate of each WT

		/*OUTPUTS*/
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double aWind_speed[]				// wind speed at each WT
	);

	void turbine_power( double fWindVelocityAtDataHeight, double fAirDensity, double *fTurbineOutput, double *fThrustCoefficient);
	void vel_delta_PQ( double fRadiiCrosswind, double fRadiiDownwind, double fTurbulenceIntensity, double fThrustCoeff, double *fNewTurbulenceIntensity, double *Vdelta);
	double wake_deficit_Park( double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dConstK, double dThrustCoeff);
	void coordtrans( double fMetersNorth, double fMetersEast, double fWind_dir_degrees, double *fMetersDownWind, double *fMetersCrosswind);
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);
	double gammaln(double x);

};

#endif
