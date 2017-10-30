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

#ifndef __lib_physics_h
#define __lib_physics_h

#include <math.h>
#include <assert.h>

namespace physics
{
	const double PI = 2.0 * acos(0.0);
	const double FT_PER_METER =			3.280839895;			// feet per meter
	const double PSI_PER_BAR =		   14.50377373066;			// psi per bar
	const double PSI_PER_INHG =			0.4911541474703;		// psi per inch of mercury
	const double Pa_PER_Atm =	   101325.00;					// pascals per atm;  101300.0 is value from FORTRAN code
	const double Pa_PER_inHg =		 3386.00;
	const double Atm_PER_Bar =			0.986923267;			// atmospheres per bar
	const double KGM3_PER_LBF3 =	   16.01846337396;			// lbs/ft^3 per kg/m^3 
	const double LB_PER_KG =			2.204622621849;			// pounds per kilogram
	const double KW_PER_HP =			0.7456998715801;		// kilowatts per unit of horsepower
	const double GRAVITY_MS2 =			9.8;					// meters per second^2; this varies between 9.78 and 9.82 depending on latitude
	const double GRAVITY_FTS2 =		   32.174;					// ft per second^2
	const double SPECIFIC_HEAT_LIQUID_WATER = 4.183 /*4.1813*/;	// J/g*K = joules per gram-degrees K; 4.183 is value currently in Fortran
	const double WATER_DENSITY =	   62.4;					// lb/ft^3
	const double R_Gas =              287.058;

	const double GAS_CONSTANT_SUPER_HEATED_STEAM =		0.461522;		// kJ/kg-K
	const double MIN_TEMP_FOR_SUPER_HEATED =		  647.073;			// deg K
	const double MIN_TEMP_FOR_STEAM1 =				  623.15;			// K

	inline double areaCircle(const double &radius) { return PI * pow(radius,2.0); }

	// temperature conversions
	inline double FarenheitToCelcius(const double &dTempInFarenheit) { return ((5.0/9.0) * (dTempInFarenheit - 32.0)); };
	inline double CelciusToFarenheit(const double &dTempInCelcius) { return (1.8 * dTempInCelcius) + 32.0; };

	inline constexpr double KelvinToCelcius(const double dTempInKelvin) { return (dTempInKelvin-273.15); }
	inline constexpr double CelciusToKelvin(const double dTempInCelcius) { return (dTempInCelcius+273.15); }
	
	inline double FarenheitToKelvin(const double &dTempInFarenheit) {return (CelciusToKelvin(FarenheitToCelcius(dTempInFarenheit))); };
	inline double KelvinToFarenheit(const double &dTempInKelvin) {return (CelciusToFarenheit(KelvinToCelcius(dTempInKelvin))); };

	// pressure conversions
	inline double AtmToPa(const double &dPressureInAtm) { return dPressureInAtm * Pa_PER_Atm; }
	inline double PaToAtm(const double &dPressureInPa) { return dPressureInPa / Pa_PER_Atm; }

	inline double InHgToPa(const double &dPressureInInchesHg) { return dPressureInInchesHg * Pa_PER_inHg; }
	inline double PaToInHg(const double &dPressureInPa) { return dPressureInPa / Pa_PER_inHg; }

	inline double mBarToAtm(const double &PressureInmBar) { return PressureInmBar * Atm_PER_Bar/1000; }
	inline double mBarToPSI(const double &PressureInmBar) { return PressureInmBar * PSI_PER_BAR/1000; }
	inline double PsiToBar(const double &psi){ return psi / PSI_PER_BAR; }

	inline double toWattHr(const double &btu) { return (btu/3.413); }
	inline double PSItoFT(const double &psi) { return psi * 144 / WATER_DENSITY; }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62.4 lb/ft^3

	bool EnthalpyFromTempAndPressure(double tempK, double pressureBar, double& enthalpy );

	const double AIR_DENSITY_SEA_LEVEL = Pa_PER_Atm/(R_Gas * CelciusToKelvin(15)); // kg/m^3 at sea level (1 atm) and 15 C
};


#endif //__lib_physics_h
