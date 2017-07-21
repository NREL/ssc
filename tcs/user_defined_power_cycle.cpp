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
#include "user_defined_power_cycle.h"
#include "csp_solver_util.h"

void C_user_defined_pc::init(const util::matrix_t<double> & T_htf_ind,
	const util::matrix_t<double> & T_amb_ind,
	const util::matrix_t<double> & m_dot_htf_ind)
{

	// Set up Linear Interp class
	int error_index = -2;
	int column_index_array[1] = {0};
	if( !mc_T_htf_ind.Set_1D_Lookup_Table( T_htf_ind, column_index_array, 1, error_index) )
	{
		if(error_index == -1)
		{
			throw(C_csp_exception("Table representing Hot HTF Temperature parametric results must have"
							"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Hot HTF Temperature must monotonically increase in the table",
							"User defined power cycle initialization"));
		}
	}

	if( !mc_T_amb_ind.Set_1D_Lookup_Table(T_amb_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing Ambient Temperature parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Ambient Temperature must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}

	if( !mc_m_dot_htf_ind.Set_1D_Lookup_Table(m_dot_htf_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing HTF mass flow rate parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The HTF mass flow rate must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}
}

double C_user_defined_pc::get_W_dot_gross_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect
	
	return get_interpolated_ND_output(i_W_dot_gross, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_Q_dot_HTF, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_W_dot_cooling, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_m_dot_water, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_interpolated_ND_output(int i_ME /*M.E. table index*/, 
							double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{

	// Y_ND = (Y_ME(T_htf)-1)*Y_INT(T_amb) + 1
	//              *
	//        (Y_ME(T_amb)-1)*Y_INT(m_dot_htf) + 1
	//              *
	//        (Y_ME(m_dot_htf)-1)*Y_INT(T_htf) + 1

	double Y_ME_T_htf = mc_T_htf_ind.interpolate_x_col_0(i_ME, T_htf_hot);
	double Y_INT_on_T_htf = mc_T_amb_ind.interpolate_x_col_0(i_ME+1, T_amb);

	double Y_ME_T_amb = mc_T_amb_ind.interpolate_x_col_0(i_ME, T_amb);
	double Y_INT_on_T_amb = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME+1, m_dot_htf_ND);

	double Y_ME_m_dot = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME, m_dot_htf_ND);
	double Y_INT_on_m_dot = mc_T_htf_ind.interpolate_x_col_0(i_ME+1, T_htf_hot);

	return ( (Y_ME_T_htf-1)*Y_INT_on_T_htf + 1.0) *
		   ( (Y_ME_T_amb-1)*Y_INT_on_T_amb + 1.0) *
		   ( (Y_ME_m_dot-1)*Y_INT_on_m_dot + 1.0);
}
