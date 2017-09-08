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

#include "fluxsim.h"
#include "solpos00.h"

void FluxSimData::Create(var_map &V)
{
    updateCalculatedParameters(V);
}

void FluxSimData::updateCalculatedParameters(var_map &V)
{
    //may need to calculate the solar position for a particular day/time, otherwise set the solar position 
	//to the input value
	double az,zen;
	if (V.flux.flux_time_type.mapval() == var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION){
		//Sun position are input, just set the corresponding values
		V.flux.flux_solar_az.Setval( V.flux.flux_solar_az_in.val );
		V.flux.flux_solar_el.Setval( V.flux.flux_solar_el_in.val );
	}
	else{
		//hour/day are provided, calculate the solar position
		int flux_day = V.flux.flux_day.val; //Day of the month
		int flux_month = V.flux.flux_month.val; //month of the year
		double flux_hour = V.flux.flux_hour.val; //hour of the day
		double lat = V.amb.latitude.val;
		double lon = V.amb.longitude.val; 
		double tmz = V.amb.time_zone.val; 

		DateTime DT;
		int doy = DT.GetDayOfYear(2011, int(flux_month), int(flux_day));
		
		//Instantiate the solpos object
		struct posdata SP, *pdat;
		pdat = &SP;	//point to structure for convenience
		S_init(pdat);		//Initialize the values

		//Calculate minutes/seconds
		double
			mins = 60.*(flux_hour - floor(flux_hour)),
			secs = 60.*(mins - floor(mins));
	
		pdat->latitude = float(lat);		//[deg] {float} North is positive
		pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
		pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
		pdat->year = 2011;		//[year] {int} 4-digit year
		pdat->month = int(flux_month);	//[mo] {int} (1-12)
		pdat->day = int(flux_day);		//[day] {int} Day of the month
		pdat->daynum = doy;	//[day] {int} Day of the year
		pdat->hour = int(flux_hour+.0001);		//[hr] {int} 0-23
		pdat->minute = int(mins);	//[min] {int} 0-59
		pdat->second = int(secs);	//[sec]	{int} 0-59
		pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


		long retcode = 0;		//Initialize with no errors
		retcode = S_solpos(pdat);	//Call the solar posotion algorithm
		S_decode(retcode, pdat);	//Check the return code

		az = SP.azim;
		zen = SP.zenetr;

		V.flux.flux_solar_az.Setval( az ); 
		V.flux.flux_solar_el.Setval( 90. - zen ); 

	}

}