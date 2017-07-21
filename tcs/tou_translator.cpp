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
#define _TCSTYPEINTERFACE_
#include "tcstype.h"

enum {	
	P_WEEKDAY_SCHEDULE,
	P_WEEKEND_SCHEDULE,

	O_TOU_VALUE,

	N_MAX };

tcsvarinfo tou_translator_variables[] = {
	// vartype            datatype              index                       name                        label                                          units           meta            group    default_value
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKDAY_SCHEDULE,         "weekday_schedule",         "12x24 matrix of values for weekdays",         "",             "",             "",          "" },
	{ TCS_PARAM,          TCS_MATRIX,           P_WEEKEND_SCHEDULE,         "weekend_schedule",         "12x24 matrix of values for weekend days",     "",             "",             "",          "" },
	
	{ TCS_OUTPUT,         TCS_NUMBER,           O_TOU_VALUE,                "tou_value",                "Value during time step",                      "",             "",             "",          "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class tou_translator : public tcstypeinterface
{
private:
	double m_hourly_tou[8760];

public:
	tou_translator( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		for (int i=0; i<8760; i++)
			m_hourly_tou[i]=0;
	}

	virtual ~tou_translator()
	{
	}

	virtual int init()
	{
		int nrows, ncols;
		double *weekdays = value( P_WEEKDAY_SCHEDULE, &nrows, &ncols );
		if ( nrows != 12 || ncols != 24 ) {
			message(TCS_ERROR,  "The TOU translator did not get a 12x24 matrix for the weekday schedule." );
			return -1;
		}
		double *weekends = value( P_WEEKEND_SCHEDULE, &nrows, &ncols );		
		if ( nrows != 12 || ncols != 24 ) {
			message( TCS_ERROR, "The TOU translator did not get a 12x24 matrix for the weekend schedule." );
			return -1;
		}

		int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

		int wday = 5, i=0;
		for (int m=0;m<12;m++)
		{
			for (int d=0;d<nday[m];d++)
			{
				bool bWeekend = (wday <= 0);

				if (wday >= 0) wday--;
				else wday = 5;

				for (int h=0; h<24 && i<8760 && m*24+h<288; h++)
				{
					if (bWeekend)
						m_hourly_tou[i] = weekends[ m*24 + h ];
					else
						m_hourly_tou[i] = weekdays[ m*24 + h ];
					i++;					
				}
			}
		}




		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		//int ihour = (int)(floor(time/3600.0 + 1.e-6)-1);
		int ihour = (int)(ceil(time / 3600.0 - 1.e-6)-1);
		if(ihour>8760-1 || ihour<0) {
			return -1;	//ERROR
		}

		double tou = m_hourly_tou[ihour];
		value( O_TOU_VALUE, tou );

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( tou_translator, "Time of Use translator", "Tom Ferguson", 1, tou_translator_variables, NULL, 0 )

