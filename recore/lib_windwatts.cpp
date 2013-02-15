#include "lib_windwatts.h"
#include "lib_physics.h"

#include <math.h>
#include "lib_util.h"

static inline double max_of(double a, double b)
{
	return (a > b) ? a : b;
}

static inline double min_of(double a, double b)
{
	return (a < b) ? a : b;
}

void wind_power_calculator::AllocateMemory()
{
	if (m_iWakeModelChoice == SIMPLE_EDDY_VISCOSITY_WAKE_MODEL)
	{
		matEVWakeDeficits.resize_fill(m_iNumberOfTurbinesInFarm, (int)(m_fMaxRotorDiameters/m_fAxialResolution)+1, 0.0); // each turbine is row, each col is wake deficit for that turbine at dist
		matEVWakeWidths.resize_fill(m_iNumberOfTurbinesInFarm, (int)(m_fMaxRotorDiameters/m_fAxialResolution)+1, 0.0); // each turbine is row, each col is wake deficit for that turbine at dist
	}
}


int wind_power_calculator::wind_power(
	// INPUTS
		double fWindSpeed,					// wind velocity m/s
		double fWindDirectionDegrees,		// wind direction 0-360, 0=N
		double dAirPressureAtm,			// barometric pressure (Atm)
		double TdryC,		// dry bulb temp ('C)
			
	// OUTPUTS
		double *FarmP,						// total farm power output
		double Power[],						// calculated power of each WT
		double Thrust[],					// thrust calculation at each WT
		double Eff[],						// downwind efficiency of each WT
		double adWindSpeed[],				// wind speed at each WT
		double aTurbulence_intensity[]		// turbulence intensity at each WT
		)
{
	if ( (m_iNumberOfTurbinesInFarm > MAX_WIND_TURBINES) || (m_iNumberOfTurbinesInFarm < 1) )
		return 0;

	size_t i,j;
	unsigned char wt_id[MAX_WIND_TURBINES], wid;

	for (i=0; i<m_iNumberOfTurbinesInFarm; i++)
		wt_id[i] = (unsigned char)i;

	// convert barometric pressure in ATM to air density
	double fAirDensity = (dAirPressureAtm * physics::Pa_PER_Atm)/(physics::R_Gas * physics::CelciusToKelvin(TdryC));   //!Air Density, kg/m^3
	double fTurbine_output, fThrust_coeff;

	// if there is only one turbine, we're done
	if (m_iNumberOfTurbinesInFarm < 2)
	{
		turbine_power(fWindSpeed, fAirDensity, &fTurbine_output, &fThrust_coeff );
		*FarmP = fTurbine_output;
		return 1;
	}

	// if power output of first turbine is zero, then it will be for the rest: we're done
	if (fTurbine_output <= 0.0)
	{
		*FarmP = 0.0;
		return m_iNumberOfTurbinesInFarm;
	}

	// ok, let's calculate the farm output
	double d, c;
	std::vector<double> aDistanceDownwind(m_iNumberOfTurbinesInFarm);	// downwind coordinate of each WT
	std::vector<double> aDistanceCrosswind(m_iNumberOfTurbinesInFarm);	// crosswind coordinate of each WT

	//!Convert to d (downwind - axial), c (crosswind - radial) coordinates and initialize others
	for (i=0;i<m_iNumberOfTurbinesInFarm;i++)
	{
		coordtrans(m_adYCoords[i], m_adXCoords[i], fWindDirectionDegrees, &d, &c );

		aDistanceDownwind[i] = d;
		aDistanceCrosswind[i] = c;

		Power[i] = 0.0;
		Thrust[i] = 0.0;
		Eff[i] = 0.0;

		adWindSpeed[i] = fWindSpeed;
		aTurbulence_intensity[i] = m_dTurbulenceIntensity;
	}

	 // Remove negative numbers from downwind, crosswind coordinates 	
 	double Dmin = aDistanceDownwind[0];
	double Cmin = aDistanceCrosswind[0];

	for (j=1;j<m_iNumberOfTurbinesInFarm;j++)
	{
		Dmin = min_of(aDistanceDownwind[j],Dmin);
		Cmin = min_of(aDistanceCrosswind[j],Cmin);
	}
	for (j=0;j<m_iNumberOfTurbinesInFarm;j++)
	{
		aDistanceDownwind[j] = aDistanceDownwind[j]-Dmin; // Final downwind coordinates, meters
		aDistanceCrosswind[j] = aDistanceCrosswind[j]-Cmin; // Final crosswind coordinates, meters
	}

	// Convert downwind, crosswind measurements from meters into wind turbine radii
	for (i=0;i<m_iNumberOfTurbinesInFarm;i++)
	{
		aDistanceDownwind[i] = 2.0*aDistanceDownwind[i]/m_dRotorDiameter;
		aDistanceCrosswind[i] = 2.0*aDistanceCrosswind[i]/m_dRotorDiameter;
	}
	
	// Record the output for the most upwind turbine (already calculated above)
	Power[0] = fTurbine_output;
	Thrust[0] = fThrust_coeff;
	Eff[0] = ( fTurbine_output < 1.0 ) ? 0.0 : 100.0;


	// Sort aDistanceDownwind, aDistanceCrosswind arrays by downwind distance, aDistanceDownwind[0] is smallest downwind distance, presumably zero
	for (j=1;j<m_iNumberOfTurbinesInFarm;j++) 
	{
		d = aDistanceDownwind[j]; // pick out each element
		c = aDistanceCrosswind[j];
		wid = wt_id[j];

		i=j;
		while (i > 0 && aDistanceDownwind[i-1] > d) // look for place to insert item
		{
			aDistanceDownwind[i] = aDistanceDownwind[i-1];
			aDistanceCrosswind[i] = aDistanceCrosswind[i-1];
			wt_id[i] = wt_id[i-1];
			i--;
		}

		aDistanceDownwind[i] = d; // insert it
		aDistanceCrosswind[i] = c;
		wt_id[i] = wid;
	}

	// run the wake model
	switch (m_iWakeModelChoice)
	{
		case PAT_QUINLAN_WAKE_MODEL:
			wake_calculations_pat_quinlan(fAirDensity, &aDistanceDownwind[0], &aDistanceCrosswind[0], Power, Thrust, Eff, adWindSpeed, aTurbulence_intensity);
			break;

		case PARK_WAKE_MODEL:
			wake_calculations_Park(fAirDensity, &aDistanceDownwind[0], &aDistanceCrosswind[0], Power, Thrust, Eff, adWindSpeed);
			break;

		case SIMPLE_EDDY_VISCOSITY_WAKE_MODEL:
			wake_calculations_EddyViscosity_Simple(fAirDensity, &aDistanceDownwind[0], &aDistanceCrosswind[0], Power, Thrust, Eff, adWindSpeed, aTurbulence_intensity);
			break;

		default:
			return 0;
	}
		
	// calculate total farm power
	*FarmP = 0;
	for (i=0;i<m_iNumberOfTurbinesInFarm;i++)
		*FarmP += Power[i];

	// Resort output arrays by wind turbine ID (0..nwt-1)
	// for consistent reporting
	double p,t,e,w,b;
	for (j=1;j<m_iNumberOfTurbinesInFarm;j++) 
	{
		p = Power[j];// pick out each element
		t = Thrust[j];
		e = Eff[j];
		w = adWindSpeed[j];
		b = aTurbulence_intensity[j];
		wid = wt_id[j];

		i=j;
		while (i > 0 && wt_id[i-1] > wid) // look for place to insert item
		{
			Power[i] = Power[i-1];
			Thrust[i] = Thrust[i-1];
			Eff[i] = Eff[i-1];
			adWindSpeed[i] = adWindSpeed[i-1];
			aTurbulence_intensity[i] = aTurbulence_intensity[i-1];
			wt_id[i] = wt_id[i-1];
			i--;
		}

		Power[i] = p;
		Thrust[i] = t;
		Eff[i] = e;
		adWindSpeed[i] = w;
		aTurbulence_intensity[i] = b;
		wt_id[i] = wid;
	}
	
	return m_iNumberOfTurbinesInFarm;
}


double wind_power_calculator::turbine_output_using_weibull(double weibull_k, double max_cp, double resource_class, double hub_efficiency[])
{	// returns same units as 'power_curve'

	double hub_ht_windspeed = pow((m_dHubHeight/50.0),m_dShearExponent) * resource_class;
	double denom = exp(gammaln(1+(1/hub_ht_windspeed)));

	double lambda = hub_ht_windspeed/denom;
	//double air_density = physics::Pa_PER_Atm * pow( (1-((0.0065*elevation)/288.0)), (physics::GRAVITY_MS2/(0.0065*287.15)) ) / (287.15*(288.0-0.0065*elevation));

	// 'RUN' MODEL ****************************************************************************************
	double total_energy_turbine=0;//, total_energy_generic=0;
	std::vector<double> weibull_cummulative(m_iLengthOfTurbinePowerCurveArray, 0);
	std::vector<double> weibull_bin(m_iLengthOfTurbinePowerCurveArray, 0);
	//std::vector<double> weibull_probability(m_iLengthOfTurbinePowerCurveArray, 0);
	std::vector<double> energy_turbine(m_iLengthOfTurbinePowerCurveArray, 0);	// energy from turbine chosen from library

	// double step = 0;
	// weibull_k = 2.10; // used for testing: this is off in the 5th significant digit when passed into SSC from samwx
	weibull_cummulative[0] = 0.0;
	weibull_bin[0] = 0.0;
	energy_turbine[0] = 0.0;
	for (size_t i=1; i<m_iLengthOfTurbinePowerCurveArray; i++)
	{
		// step = (i) ? wind_speed[i]-wind_speed[i-1] : 0;

		// calculate Weibull likelihood of the wind blowing in the range from windspeed[i-1] to windspeed[i]
		weibull_cummulative[i] = 1.0 - exp(-pow(m_adPowerCurveWS[i]/lambda,weibull_k));
		weibull_bin[i] = weibull_cummulative[i] - weibull_cummulative[i-1];
		// THIS IS NOT FOR THE BIN wind speed[i to i-1]: weibull_probability[i] = ( (weibull_k / pow(lambda,weibull_k)) * pow(wind_speed[i],(weibull_k - 1)) * exp(-pow(wind_speed[i]/lambda,weibull_k)) );

		// calculate annual energy from turbine at this wind speed = (hours per year at this wind speed) X (turbine output at wind speed)
		energy_turbine[i] = (8760.0 * weibull_bin[i]) * m_adPowerCurveKW[i];

		// keep track of cummulative output
		total_energy_turbine += energy_turbine[i];
	}

	// calculate output accounting for losses
	return  total_energy_turbine;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Private functions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void wind_power_calculator::wake_calculations_pat_quinlan(
	/*INPUTS*/
	double air_density,
	double aDistanceDownwind[],			// downwind coordinate of each WT
	double aDistanceCrosswind[],		// crosswind coordinate of each WT

	/*OUTPUTS*/
	double Power[],						// calculated power of each WT
	double Thrust[],					// thrust calculation at each WT
	double Eff[],						// downwind efficiency of each WT
	double adWindSpeed[],				// wind speed at each WT
	double aTurbulence_intensity[]		// turbulence intensity at each WT
)
{
	// we calculated the output of the most upwind turbine above, now we have to go through the remaining turbines in the farm
	// the i loop is going through the turbines, from first (most upwind) to last (most downwind)
	// for each turbine i, if the output of all upwind turbines has already been calculated (j = i+1),
	// then set this turbine's output.
	// then go through the j loop, calculating this turbine's impact on the wind speed at each downwind turbine

	for (size_t i=0; i<m_iNumberOfTurbinesInFarm-1; i++) // upwind turbines
	{
		for (size_t j=i+1; j<m_iNumberOfTurbinesInFarm; j++) // downwind turbines
		{
			// Wake Model: calculate downwind propagation of wind speed reduction due to upwind turbines

			// All distances in these calculations have already been converted into units of wind turbine blade radii

			// distance downwind = distance from turbine i to turbine j along axis of wind direction
			double fDistanceDownwind = aDistanceDownwind[j] - aDistanceDownwind[i]; 

			// separation crosswind between turbine i and turbine j

			// EQN SIMPLIFIED B/C all hub heights the same currently
			//  F: RR(j) = ((DA(4,j)-DA(4,i))**2.0+(DA(5,j)-DA(5,i))**2.0)**0.5
			//
			//  C: rr    = sqrt((aDistanceCrosswind[j]-aDistanceCrosswind[i])*(aDistanceCrosswind[j]-aDistanceCrosswind[i]) + (HtRad[j]-HtRad[i])*(HtRad[j]-HtRad[i]));
			//    where HtRad = HubHt/Rotor_Di for each WT
			double fDistanceCrosswind = aDistanceCrosswind[j] - aDistanceCrosswind[i];
				
			// Calculate the wind speed reduction and turbulence at turbine j, due to turbine i
			// turbulence coeff for turbine j will be impacted (always added to) by all the upwind turbines based on how far upwind they are.
			double fTurbIntensity, delt;
			vel_delta_PQ( fDistanceCrosswind, fDistanceDownwind, aTurbulence_intensity[j], Thrust[i], &fTurbIntensity, &delt);
			if (delt>1.0) delt = 1.0;
			adWindSpeed[j] = adWindSpeed[j]*(1.0-delt);
			aTurbulence_intensity[j] = fTurbIntensity;

			// when j = i+1, (first time through the j loop for each i loop) that means all the turbines upwind 
			// of this one (j) have already had their outputs calculated
			// so now we'll set this turbine's output, then we can calculate its contribution (wake impacts) for all
			// of the downwind (j >= i+2) turbines
			if (j==i+1)
			{
				double fTurbine_output=0, fThrust_coeff=0;
				turbine_power( adWindSpeed[j], air_density,  &fTurbine_output, &fThrust_coeff);
				Power[j] = fTurbine_output;
				Thrust[j] = fThrust_coeff;

				if (Power[0] < 0.0)
					Eff[j] = 0.0;
				else
					Eff[j] = 100.0*(fTurbine_output+0.0001)/(Power[0]+0.0001);
			}

		}
	} 
}

void wind_power_calculator::wake_calculations_Park(
	/*INPUTS*/
	double air_density,
	double aDistanceDownwind[],			// downwind coordinate of each WT in radii from upwind turbine
	double aDistanceCrosswind[],		// crosswind coordinate of each WT in radii from upwind turbine

	/*OUTPUTS*/
	double Power[],						// calculated power of each WT
	double Thrust[],					// thrust calculation at each WT
	double Eff[],						// downwind efficiency of each WT
	double adWindSpeed[]				// wind speed at each WT
)
{
	double dTurbineRadius = m_dRotorDiameter/2;

	for (size_t i=1; i<m_iNumberOfTurbinesInFarm; i++) // downwind turbines, i=0 has already been done
	{
		double dDeficit = 0;
		for (size_t j=0; j<i; j++) // upwind turbines
		{
			// distance downwind = distance from turbine i to turbine j along axis of wind direction
			double fDistanceDownwindMeters = dTurbineRadius*fabs(aDistanceDownwind[i] - aDistanceDownwind[j]); 

			// separation crosswind between turbine i and turbine j
			double fDistanceCrosswindMeters = dTurbineRadius*fabs(aDistanceCrosswind[i] - aDistanceCrosswind[j]);
				
			// Calculate the wind speed reduction at turbine i, due to all upwind turbines [j]
			// The Park model uses the max deficit from all the upwind turbines and uses that to calculate the wind speed at this downwind turbine
			// It ignores all other deficits
			dDeficit = max_of(dDeficit, wake_deficit_Park(fDistanceCrosswindMeters, fDistanceDownwindMeters, dTurbineRadius, dTurbineRadius, Thrust[j]));

		}
		// use the max deficit found to calculate the turbine output
		adWindSpeed[i] = adWindSpeed[i]*(1-dDeficit);

		double fTurbine_output=0, fThrust_coeff=0;
		turbine_power(adWindSpeed[i], air_density,  &fTurbine_output, &fThrust_coeff);
		Power[i] = fTurbine_output;
		Thrust[i] = fThrust_coeff;

		if (Power[0] < 0.0)
			Eff[i] = 0.0;
		else
			Eff[i] = 100.0*(fTurbine_output+0.0001)/(Power[0]+0.0001);
	} 
}


// Implements a simplified Eddy-Viscosity model as per "Simplified Soultion To The Eddy Viscosity Wake Model" - 2009 by Dr Mike Anderson of RES
void wind_power_calculator::wake_calculations_EddyViscosity_Simple(
	/*INPUTS*/
	double air_density,
	double aDistanceDownwind[],			// downwind coordinate of each WT in radii from upwind turbine
	double aDistanceCrosswind[],		// crosswind coordinate of each WT in radii from upwind turbine

	/*OUTPUTS*/
	double Power[],						// calculated power of each WT
	double Thrust[],					// thrust calculation at each WT
	double Eff[],						// downwind efficiency of each WT
	double adWindSpeed[],				// wind speed at each WT
	double aTurbulence_intensity[]		// turbulence intensity at each WT
)
{
	double dTurbineRadius = m_dRotorDiameter/2;
	matEVWakeDeficits.fill(0.0);
	matEVWakeWidths.fill(0.0);
	std::vector<VMLN> vmln(m_iNumberOfTurbinesInFarm);
	std::vector<double> Iamb(m_iNumberOfTurbinesInFarm, m_dTurbulenceIntensity);

	// Note that this 'i' loop starts with i=0, which is necessary to initialize stuff for turbine[0]
	for (size_t i=0; i<m_iNumberOfTurbinesInFarm; i++) // downwind turbines, but starting with most upwind and working downwind, i=0 has already been done
	{
		double dDeficit = 0, Iadd = 0, dTotalTI = aTurbulence_intensity[i];
		double dTOut=0, dThrustCoeff=0;
		for (size_t j=0; j<i; j++) // upwind turbines - turbines upwind of turbine[i]
		{
			// distance downwind = distance from turbine i to turbine j along axis of wind direction
			double dDistAxialInDiameters = fabs(aDistanceDownwind[i] - aDistanceDownwind[j])/2.0;

			// separation crosswind between turbine i and turbine j
			double dDistRadialInDiameters = fabs(aDistanceCrosswind[i] - aDistanceCrosswind[j])/2.0;
			
			double dWakeRadiusMeters = GetEVWakeWidth(j, dDistAxialInDiameters);  // the radius of the wake
			if (dWakeRadiusMeters<=0)
				continue;

			// calculate the wake deficit
			double dDef = wake_deficit_EV(j, dDistRadialInDiameters, dDistAxialInDiameters);
			double dWindSpeedWaked = adWindSpeed[0] * (1 - dDef); // wind speed = free stream * (1-deficit)

			// keep it if it's bigger
			dDeficit = max_of(dDeficit, dDef);

			Iadd = added_turbulence_intensity(Iamb[j], Thrust[j], dDistAxialInDiameters*m_dRotorDiameter, vmln[j]);
			
			double dFractionOfOverlap = simple_intersect(dDistRadialInDiameters*m_dRotorDiameter, dTurbineRadius, dWakeRadiusMeters);
			dTotalTI = max_of(dTotalTI, GetTotalTI(aTurbulence_intensity[i], Iadd, adWindSpeed[0], dWindSpeedWaked, dFractionOfOverlap));
		}
		// use the max deficit found to calculate the turbine output
		adWindSpeed[i] = adWindSpeed[0]*(1-dDeficit);
		aTurbulence_intensity[i] = dTotalTI;
		turbine_power(adWindSpeed[i], air_density,  &Power[i], &Thrust[i]);

		if (Power[0] < 0.0)
			Eff[i] = 0.0;
		else
			Eff[i] = 100.0*(Power[i]+0.0001)/(Power[0]+0.0001);

		// now that turbine i wind speed, output, thrust, etc. have been calculated, calculate wake characteristics for it, so downwind turbines can use info


	} 
}

double wind_power_calculator::wake_deficit_EV(int iUpwindTurbine, double dDistCrossWind, double dDistDownWind)
{
	double dDef = this->GetEVVelocityDeficit(iUpwindTurbine, dDistDownWind);
	if(dDef <= 0.0)
		return 0.0;

	double dSteps = 25.0;
	double dCrossWindDistanceInMeters = dDistCrossWind * m_dRotorDiameter;
	double dWidth = GetEVWakeWidth(iUpwindTurbine, dDistDownWind);
	double dRadius = m_dRotorDiameter/2.0;
	double dStep = m_dRotorDiameter/dSteps;

	double dTotal = 0.0;
	for(double y=dCrossWindDistanceInMeters-dRadius; y<=dCrossWindDistanceInMeters+dRadius; y+=dStep)
	{
		dTotal += dDef * exp(-3.56*(((y*y))/(dWidth*dWidth)));  // exp term ranges from >zero to one
	}	

	dTotal /= (dSteps+1.0); // average of all terms above will be zero to dDef

	return dTotal;
}


double wind_power_calculator::GetEVWakeWidth(int iUpwindTurbine, double dAxialDistanceInDiameters)
{	// get the wake width from the upwind turbine's array describing its wake
	// based on the way the result is used in openWind, the wake widths stored in matEVWakeWidths must be a radial distance from the centerline

	// if we're too close, it's just the initial wake width
	double dDistPastMin = dAxialDistanceInDiameters - MIN_DIAM_EV; // in diameters
	if (dDistPastMin < 0.0)
		return m_dRotorDiameter * matEVWakeWidths.at(iUpwindTurbine,0);

	double dDistInResolutionUnits = dDistPastMin / m_fAxialResolution;
	int iLowerIndex = (int)dDistInResolutionUnits;
	size_t iUpperIndex = iLowerIndex+1;
	dDistInResolutionUnits -= iLowerIndex;
	
	if(iUpperIndex >= matEVWakeWidths.ncols())
		return 0.0;

	return m_dRotorDiameter * max_of(1.0,(  matEVWakeWidths.at(iUpwindTurbine, iLowerIndex) * (1.0-dDistInResolutionUnits) + matEVWakeWidths.at(iUpwindTurbine, iUpperIndex) * dDistInResolutionUnits));	// in meters
}

double wind_power_calculator::GetEVVelocityDeficit(int iUpwindTurbine, double dAxialDistanceInDiameters)
{	// get the wake width from the upwind turbine's array describing its wake

	// if we're too close, it's just the initial wake width
	double dDistPastMin = dAxialDistanceInDiameters - MIN_DIAM_EV; // in diameters
	if (dDistPastMin < 0.0)
		return m_dRotorDiameter * matEVWakeDeficits.at(iUpwindTurbine,0);

	double dDistInResolutionUnits = dDistPastMin / m_fAxialResolution;
	int iLowerIndex = (int)dDistInResolutionUnits;
	size_t iUpperIndex = iLowerIndex+1;
	dDistInResolutionUnits -= iLowerIndex;
	
	if(iUpperIndex >= matEVWakeDeficits.ncols())
		return 0.0;

	return (matEVWakeDeficits.at(iUpwindTurbine, iLowerIndex) * (1.0-dDistInResolutionUnits) ) + (matEVWakeDeficits.at(iUpwindTurbine, iUpperIndex) * dDistInResolutionUnits);	// in meters
}

double wind_power_calculator::added_turbulence_intensity(double dTIAtUpstreamTurbine, double Ct,double deltaX, VMLN& vmln)
{
	// Xn is in meters
	double Xn = max_of(0.0000000001,vmln.Xn);
	
	// this formula can be found in Wind Energy Handbook by Bossanyi, page 36
//	double Iadd = 5.7*pow(Ct,0.7)*pow(dTIAtUpstreamTurbine, 0.68)*pow(deltaX/Xn,-0.96);
	double Iadd = 5.7*pow(Ct,0.7)*pow(dTIAtUpstreamTurbine, 0.68)*pow(max_of(1.5, deltaX/Xn),-0.96);// limits X>=Xn
	
	return max_of(0.0,Iadd);
}

double wind_power_calculator::GetTotalTI(double ambientTI, double additionalTI, double Uo, double Uw, double partial)
{
	if(Uw<=0.0)
		return ambientTI;

	double f = max_of(0.0,ambientTI*ambientTI + additionalTI*additionalTI);
	f = sqrt(f)*Uo/Uw;
	return (1.0-partial)*ambientTI + partial*f;
//	return f;
}

void wind_power_calculator::turbine_power( double fWindVelocityAtDataHeight, double fAirDensity, double *fTurbineOutput, double *fThrustCoefficient )
{
	// default outputs to zero
	*fThrustCoefficient = 0;
	*fTurbineOutput = 0;

	// If the wind speed measurement height (fDataHeight) differs from the turbine hub height (Hub_Ht), use the shear to correct it. 
	if (m_dShearExponent > 1.0) m_dShearExponent = 1.0/7.0;
	double fWindSpeedAtHubHeight = fWindVelocityAtDataHeight * pow(m_dHubHeight/m_dMeasurementHeight, m_dShearExponent);
	
	// Find power from turbine power curve
	double out_pwr=0.0;
	if ( (fWindSpeedAtHubHeight > m_adPowerCurveWS[0]) && (fWindSpeedAtHubHeight < m_adPowerCurveWS[m_iLengthOfTurbinePowerCurveArray-1]) ) 
	{
		int j = 1;
		while ( m_adPowerCurveWS[j] <= fWindSpeedAtHubHeight )
			j++; // find first m_adPowerCurveWS > fWindSpeedAtHubHeight

		out_pwr = util::interpolate(m_adPowerCurveWS[j-1], m_adPowerCurveKW[j-1], m_adPowerCurveWS[j], m_adPowerCurveKW[j], fWindSpeedAtHubHeight);
	}
	else if (fWindSpeedAtHubHeight >= m_adPowerCurveWS[m_iLengthOfTurbinePowerCurveArray-1]) 
		out_pwr = m_adPowerCurveKW[m_iLengthOfTurbinePowerCurveArray-1]; // wind speed greater than maximum in the power curve: power output is last value

	// Check against turbine cut-in speed
	if ( fWindSpeedAtHubHeight < m_dCutInSpeed) out_pwr = 0.0; 

	// wind turbine output corrected for site air density
	out_pwr *= fAirDensity/physics::AIR_DENSITY_SEA_LEVEL;

	// stall control (Ctl_Mode == 2) defaults to simple density ratio
	if ( (m_iControlMode == 1)/*pitch control*/ || (m_iControlMode == 0)/*var speed control*/ )
	{
		double NewVRat = m_dRatedSpeed * pow(fAirDensity/physics::AIR_DENSITY_SEA_LEVEL, 1.0/3.0);
		if (out_pwr > m_dRatedPower)
			out_pwr = m_dRatedPower;
		else if (fWindSpeedAtHubHeight > NewVRat)
			out_pwr = m_dRatedPower;
	}

	// if calculated power is > 1% of rating, set outputs
	if (out_pwr > (m_dRatedPower * 0.01))
	{
		out_pwr = out_pwr*(1.0-m_dLossesPercent) - m_dLossesAbsolute;
		double pden = 0.5*fAirDensity*pow(fWindSpeedAtHubHeight, 3.0);
		double area = physics::PI/4.0*m_dRotorDiameter*m_dRotorDiameter;
		double fPowerCoefficient = max_of( 0.0, 1000.0*out_pwr/(pden*area) );

		// set outputs to something other than zero
		*fTurbineOutput = out_pwr;
		if (fPowerCoefficient >= 0.0)
			*fThrustCoefficient = max_of( 0.0, -1.453989e-2 + 1.473506*fPowerCoefficient - 2.330823*pow(fPowerCoefficient,2) + 3.885123*pow(fPowerCoefficient,3) );
	}
	return;
}

// wake modeling - calculating the change in wind speed due to wake effects of upwind turbine
void wind_power_calculator::vel_delta_PQ( double fRadiiCrosswind, double fRadiiDownwind, double fTurbulenceIntensity, double fThrustCoeff, double *fNewTurbulenceIntensity, double *Vdelta)
{
	// vel_delta_loc() can return values > 1 for Vdelta, which leads to negative wind speed at turbine j
	// turbine_power() will handle a negative wind speed by setting power output, thrust, and turbulence to zero
	// This seems prone to errors, Wind[j] should probably be limited to values >=0

	// fRadiiCrosswind = distance from being straight downwind (distance 'crosswind') of the upwind turbine, measured in the number of upwind turbine radii
	// fRadiiDownwind = Distance Downwind, measured in the number of upwind turbine radii (e.g. 2.5 = 2.5 times the radius of the upwind turbine)
	// fTurbulenceIntensity = turbulence intensity
	// fThrustCoeff = Thrust coeff

	// The calculated value of fNewTurbulenceIntensity, passed out of function, will then be sent back in as the new fTurbulenceIntensity input the next time
	// this function is called for the same turbine.  So, fTurbulenceIntensity can only increase due to influence of other (upwind) turbines,
	// based on the equation for fNewTurbulenceIntensity (square root of two numbers squared, where one number is fTurbulenceIntensity)
	// Note that the calculation of fNewTurbulenceIntensity does NOT include fRadiiCrosswind - so it's only influenced by how far upwind the other turbine is,
	// EVEN IF IT'S 19 ROTOR RADII TO THE SIDE!!!

	if (fRadiiCrosswind > 20.0 || fTurbulenceIntensity <= 0.0 || fRadiiDownwind <= 0.0 || fThrustCoeff <= 0.0)
	{
		*fNewTurbulenceIntensity = fTurbulenceIntensity;
		*Vdelta = 0.0;
	}
	else
	{
		double fAddedTurbulence = (fThrustCoeff/7.0)*(1.0-(2.0/5.0)*log(2.0*fRadiiDownwind)); // NOTE that this equation does not account for how far over the turbine is!!
		*fNewTurbulenceIntensity = sqrt( pow(fAddedTurbulence,2.0) + pow(fTurbulenceIntensity,2.0) );

		double AA = pow(*fNewTurbulenceIntensity,2.0) * pow(fRadiiDownwind,2.0);
		double fExp = max_of( -99.0, (-pow(fRadiiCrosswind,2.0)/(2.0*AA)) );
		*Vdelta = (fThrustCoeff/(4.0*AA))*exp(fExp);
	}
}

// wake modeling - Park model used to calculate the change in wind speed due to wake effects of upwind turbine
double wind_power_calculator::wake_deficit_Park( double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff)
{	// return the wind speed deficit due to wake effects. 0=no deficit, up to 1 = 100%
	if (dThrustCoeff>1)
		return 0;

	double dRadiusOfWake = (dRadiusUpstream) + m_dWakeDecayCoefficient * dDistDownWind; // radius of circle formed by wake from upwind rotor
	double dAreaOverlap = circle_overlap(dDistCrossWind, dRadiusDownstream, dRadiusOfWake);
	return (1 - sqrt(1-dThrustCoeff)) * pow(dRadiusUpstream/dRadiusOfWake,2) * (dAreaOverlap/(physics::PI*dRadiusDownstream*dRadiusDownstream));
}


// This subroutine transforms the map east,north coordinate system to a downwind,crosswind orientation orthogonal to current wind direction.
void wind_power_calculator::coordtrans( double fMetersNorth, double fMetersEast, double fWind_dir_degrees, double *fMetersDownWind, double *fMetersCrosswind)
{
	// rotate wind direction to match unit circle (where zero is East, not North)
	fWind_dir_degrees += 90;

	// convert degrees to radians
	double fWind_dir_radians = fWind_dir_degrees*physics::PI/180.0;

	// create downwind and crosswind coordinates
	*fMetersDownWind = fMetersEast*cos(fWind_dir_radians) - ( fMetersNorth * sin(fWind_dir_radians) ); //!northerly = FROM North"
	*fMetersCrosswind = fMetersEast*sin(fWind_dir_radians) + ( fMetersNorth * cos(fWind_dir_radians) );
}

double wind_power_calculator::circle_overlap(double dist_center_to_center, double rad1, double rad2)
{	// returns the area of overlap, NOT a fraction
	if (dist_center_to_center<0 || rad1<0 || rad2<0)
		return 0;

	if (dist_center_to_center > rad1+rad2)
		return 0;

	if (rad1 >= dist_center_to_center + rad2)
		return physics::PI * pow(rad2,2); // overlap = area of circle 2

	if (rad2 >= dist_center_to_center + rad1)
		return physics::PI * pow(rad1,2); // overlap = area of circle 1 ( if rad1 is turbine, it's completely inside wake)

	double t1 = pow(rad1,2) * acos( (pow(dist_center_to_center,2) + pow(rad1,2) - pow(rad2,2) )/(2*dist_center_to_center*rad1));
	double t2 = pow(rad2,2) * acos( (pow(dist_center_to_center,2) + pow(rad2,2) - pow(rad1,2) )/(2*dist_center_to_center*rad2));
	double t3 = 0.5 * sqrt( (-dist_center_to_center+rad1+rad2) * (dist_center_to_center+rad2-rad2) * (dist_center_to_center-rad1+rad2) * (dist_center_to_center+rad1+rad2) );

	return t1+t2-t3;
}

double wind_power_calculator::simple_intersect(double dist_center_to_center, double dRadiusTurbine, double dRadiusWake)
{	// returns the fraction of overlap, NOT an area
	if (dist_center_to_center<0 || dRadiusTurbine<0 || dRadiusWake<0)
		return 0;

	if (dist_center_to_center > dRadiusTurbine+dRadiusWake)
		return 0;

	if (dRadiusWake >= dist_center_to_center + dRadiusTurbine)
		return 1; // turbine completely inside wake

	return min_of(1.0,max_of(0.0,(dRadiusTurbine+dRadiusWake-dist_center_to_center)/(2*dRadiusTurbine)));
}

double wind_power_calculator::gammaln(double x)
{
    // Based on VBA code in Xnumbers.xla v 5.6
	// by Foxes Team, 2007
    // E -mail: leovlp@libero.it
    // Web:    http://digilander.libero.it/foxes
	// 10.11.2006

	double z, w, s, p, mantissa, expo;
	std::vector<double> cf(15);
	const double DOUBLEPI = 2 * physics::PI;
    const double G_ = 607.0/128.0; //= 4.7421875
    
    z = x - 1;
    cf[0] = 0.999999999999997;
    cf[1] = 57.1562356658629;
    cf[2] = -59.5979603554755;
    cf[3] = 14.1360979747417;
    cf[4] = -0.49191381609762;
    cf[5] = 3.39946499848119E-05;
    cf[6] = 4.65236289270486E-05;
    cf[7] = -9.83744753048796E-05;
    cf[8] = 1.58088703224912E-04;
    cf[9] = -2.10264441724105E-04;
    cf[10] = 2.17439618115213E-04;
    cf[11] = -1.64318106536764E-04;
    cf[12] = 8.44182239838528E-05;
    cf[13] = -2.61908384015814E-05;
    cf[14] = 3.68991826595316E-06;
    
    w = exp(G_)/sqrt(DOUBLEPI);
    s = cf[0];

	for(int i=1; i<15; i++){
        s += cf[i] / (z + i);
	}
    s = s / w;
    p = log((z + G_ + 0.5) / exp(1.0)) * (z + 0.5) / log(10.0);
    
	//split in mantissa and exponent to avoid overflow
    expo = floor(p);
    p = p - floor(p);
    mantissa = pow(10, p) * s;
    
	//rescaling
    p = floor(log(mantissa) / log(10.0));  // 'int' replaced with '' since VBA 'int' rounds negative numbers down
    mantissa = mantissa * pow(10.0, -p);
    expo = expo + p;

	return log(mantissa) + expo * log(10.0);
}

