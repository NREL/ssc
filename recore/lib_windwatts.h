#ifndef __lib_windwatts_h
#define __lib_windwatts_h

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
	double m_dCutInSpeed;			// wind speed Cut in (m/s)
	double m_dRatedSpeed;			// rated wind speed
	double m_dRatedPower;			// rated power (kw)
	double m_dLossesAbsolute;		// constant loss
	double m_dLossesPercent;		// loss as percent

	int GetMaxTurbines() {return MAX_WIND_TURBINES;}

	int wind_power(
		// INPUTS
			double Vel_T,    // wind velocity m/s
			double Theta_T,  // wind direction 0-360, 0=N
			double BarPAtm,  // barometric pressure (Atm)
			double TdryC,    // dry bulb temp ('C)
			double WT_x[],   // x coordinates of wind turbines
			double WT_y[],   // y coordinates of wind turbines
			double PC_w[],   // Power curve wind speeds m/s
			double PC_p[],   // Power curve output power (kW)
			
		// OUTPUTS
			double *FarmP,   // total farm power output
			double Dn[],     // downwind coordinate of each WT
			double Cs[],     // crosswind coordinate of each WT
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
		double wind_speed[], 
		double power_curve[], 
		double hub_efficiency[]
	);

private:
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);
	void turbine_power( double fWindVelocityAtDataHeight, double fAirDensity, double afPowerCurve_Speeds[], double afPowerCurve_TurbineOutputs[], double *fTurbineOutput, double *fThrustCoefficient);
	void vel_delta_loc( double fRadiiCrosswind, double fRadiiDownwind, double fTurbulenceIntensity, double fThrustCoeff, double *fNewTurbulenceIntensity, double *Vdelta);
	void vel_delta_Park( double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dConstK, double dThrustCoeff, double &dVdelta);
	void coordtrans( double fMetersNorth, double fMetersEast, double fWind_dir_degrees, double *fMetersDownWind, double *fMetersCrosswind);
	double gammaln(double x);

};

#endif
