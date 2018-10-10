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

#include <math.h>
#include <cmath>
#include <limits>
#include <stddef.h>
#include "lib_pvinv.h"



partload_inverter_t::partload_inverter_t( )
{
	Paco = Pdco = Pntare = std::numeric_limits<double>::quiet_NaN();
}

bool partload_inverter_t::acpower(
	/* inputs */
	double Pdc,     /* Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

								 /* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
)
{
	//pass through inputs to the multiple MPPT function as an array with only one entry
	std::vector<double> Pdc_vec;
	Pdc_vec.push_back(Pdc);
	if (!acpower(Pdc_vec, Pac, Ppar, Plr, Eff, Pcliploss, Pntloss))
		return false;

	return true;
}

bool partload_inverter_t::acpower(
	/* inputs */
	std::vector<double> Pdc,     /* Vector of Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

	/* outputs */
	double *Pac,    /* AC output power (Wac) */
	double *Ppar,   /* AC parasitic power consumption (Wac) */
	double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
	double *Eff,	    /* Conversion efficiency (0..1) */
	double *Pcliploss, /* Power loss due to clipping loss (Wac) */
	double *Pntloss /* Power loss due to night time tare loss (Wac) */
	)
{
	double Pdc_total = 0;
	for (size_t m = 0; m < Pdc.size(); m++)
		Pdc_total += Pdc[m];
	if ( Pdco <= 0 ) return false;

	// handle limits - can send error back or record out of range values
//	if ( Pdc < 0 ) Pdc = 0;
//	if ( Pdc > Pdco ) Pdc = Pdco;

	// linear interpolation based on Pdc/Pdco and *Partload and *Efficiency arrays
	double x = 100.0 * Pdc_total / Pdco; // percentages in partload ratio

	int n = (int)Partload.size();

	bool ascnd = (Partload[n-1] > Partload[0]); // check ascending order
	int ndx;
	int nu = n;
	int nl = 0;

	// Numerical Recipes in C p.117
	while ( (nu-nl) > 1 )
	{
		ndx = (nu + nl) >> 1; // divide by 2
		if ( (x >= Partload[ndx]) == ascnd )
			nl = ndx;
		else 
			nu = ndx;
	}
	if ( x == Partload[0] )
		ndx = 0;
	else if ( x == Partload[n-1] )
		ndx = n-1;
	else
		ndx = nl;

	// check in range
	if (ndx >= (n-1))
		ndx = n-2;
	if ( ndx < 0 ) 
		ndx =0;

	// x between Partload[ndx] and Partload[ndx-1]
	if ( x > Partload[ndx] )
		*Eff = Efficiency[ndx] + ((Efficiency[ndx+1] - Efficiency[ndx]) / 
									(Partload[ndx+1] - Partload[ndx] )) * (x - Partload[ndx]);
	else
		*Eff = Efficiency[ndx];

	if ( *Eff < 0.0 ) *Eff = 0.0;

	*Eff /= 100.0; // user data in percentages

	*Pac = *Eff * Pdc_total;
	*Ppar = 0.0;

	// night time power loss Wac
	*Pntloss = 0.0;
	if (Pdc_total <= 0.0)
	{
		*Pac = -Pntare;
		*Ppar = Pntare;
		*Pntloss = Pntare;
	}

	// clipping loss Wac
	*Pcliploss = 0.0;
	double PacNoClip = *Pac;
	if ( *Pac > Paco )	
	{
		*Pac = Paco;
		*Pcliploss = PacNoClip - *Pac;
	}

	*Plr = Pdc_total / Pdco;

	return true;
}



