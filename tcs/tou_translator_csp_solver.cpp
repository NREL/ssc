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

#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_util.h"
#include "csp_solver_core.h"

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
C_csp_tou_block_schedules mc_tou;
C_csp_tou::S_csp_tou_outputs ms_outputs;

public:
	tou_translator( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{

	}

	virtual ~tou_translator()
	{
	}

	virtual int init()
	{
		int nrows, ncols;
		mc_tou.ms_params.mc_csp_ops.mc_weekdays.resize(nrows,ncols);
		for( int r = 0; r < nrows; r++ )
			for( int c = 0; c < ncols; c++ )
				mc_tou.ms_params.mc_csp_ops.mc_weekdays(r, c) = TCS_MATRIX_INDEX(var(P_WEEKDAY_SCHEDULE), r, c);

		nrows = ncols = 0;
		mc_tou.ms_params.mc_csp_ops.mc_weekends.resize(nrows, ncols);
		for( int r = 0; r < nrows; r++ )
			for( int c = 0; c < ncols; c++ )
				mc_tou.ms_params.mc_csp_ops.mc_weekends(r, c) = TCS_MATRIX_INDEX(var(P_WEEKEND_SCHEDULE), r, c);

		// Hardcode member data that TCS models don't use but class requires
		mc_tou.ms_params.mc_pricing.mc_weekdays = mc_tou.ms_params.mc_csp_ops.mc_weekdays;
		mc_tou.ms_params.mc_pricing.mc_weekends = mc_tou.ms_params.mc_csp_ops.mc_weekends;

		std::vector<double> turbine_fracs;
		turbine_fracs.resize(9,0.0);

		mc_tou.ms_params.mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC] = turbine_fracs;
		mc_tou.ms_params.mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE] = turbine_fracs;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			mc_tou.init();
		}
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg))
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}
			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		

		return 0;
	}

	virtual int call( double time, double , int )
	{
		
		int out_type = -1;
		std::string out_msg = "";
		
		try
		{
			mc_tou.call(time, ms_outputs);
		}

		
		catch(C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}
			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( mc_tou.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		value(O_TOU_VALUE, (double)ms_outputs.m_csp_op_tou);

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( tou_translator, "Time of Use translator", "Tom Ferguson", 1, tou_translator_variables, NULL, 0 )
