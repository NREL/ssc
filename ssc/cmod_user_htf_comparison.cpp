/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
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

#include "core.h"
#include "htf_props.h"



static var_info _cm_vtab_user_htf_comparison[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "HTF_code1",     "HTF fluid code: Fluid 1",                                 "-",       "",    "",      "*",     "",                ""  },
	{ SSC_INPUT,  SSC_MATRIX,  "fl_props1",     "User defined field fluid property data, Fluid 1",         "-",       "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },
	
	{ SSC_INPUT,  SSC_NUMBER,  "HTF_code2",     "HTF fluid code: Fluid 2",                                 "-",       "",    "",      "*",     "",                ""  },
	{ SSC_INPUT,  SSC_MATRIX,  "fl_props2",     "User defined field fluid property data, Fluid 2",         "-",       "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "are_equal",     "1: Input tables are equal, 0: not equal",                 "-",       "",                                                         "", "*", "", "" },

	var_info_invalid };

class cm_user_htf_comparison : public compute_module
{
public:

	cm_user_htf_comparison()
	{
		add_var_info(_cm_vtab_user_htf_comparison);
	}

	void exec() throw(general_error)
	{
		// Get fluid codes
		int htf_code1 = (int)as_double("HTF_code1");
		int htf_code2 = (int)as_double("HTF_code2");

		int htf_user_defined_code = HTFProperties::User_defined;

		if( htf_code1 != htf_code2 )
		{
			assign("are_equal", 0.0);
			return;
		}

		// **********************************************
		// Following code assumes HTF code 1 = HTF code 2
		// **********************************************

		// HTF codes are equal, so are they user defined HTFs?
		if( htf_code1 != htf_user_defined_code)
		{
			assign("are_equal", 1.0);
			return;
		}

		HTFProperties htfProps1;			// Instance of HTFProperties class for receiver/HX htf

		size_t nrows = 0, ncols = 0;
		ssc_number_t *fl_props1 = as_matrix("fl_props1", &nrows, &ncols);
		if( fl_props1 != 0 && nrows > 2 && ncols == 7 )
		{
			util::matrix_t<ssc_number_t> mat;
			mat.assign(fl_props1, nrows, ncols);

			util::matrix_t<double> mat_double(nrows, ncols);
			for( int i = 0; i < nrows; i++ )
			{
				for( int j = 0; j < ncols; j++ )
				{
					mat_double(i, j) = (double)mat(i, j);
				}
			}
			if( !htfProps1.SetUserDefinedFluid(mat_double) )
			{
				// If table doesn't read, assumes HTFs are not equal (as they will fail in performance code)
				assign("are_equal", 0.0);
				return;
			}
		}
		else
		{
			// If tables isn't in correct format, assumes HTFs are not equal (as they will fail in performance code)
			assign("are_equal", 0.0);
			return;
		}

		HTFProperties htfProps2;			// Instance of HTFProperties class for receiver/HX htf

		nrows = 0; 
		ncols = 0;
		ssc_number_t *fl_props2 = as_matrix("fl_props2", &nrows, &ncols);
		if( fl_props2 != 0 && nrows > 2 && ncols == 7 )
		{
			util::matrix_t<ssc_number_t> mat;
			mat.assign(fl_props2, nrows, ncols);

			util::matrix_t<double> mat_double(nrows, ncols);
			for( int i = 0; i < nrows; i++ )
			{
				for( int j = 0; j < ncols; j++ )
				{
					mat_double(i, j) = (double)mat(i, j);
				}
			}
			if( !htfProps2.SetUserDefinedFluid(mat_double) )
			{
				// If table doesn't read, assumes HTFs are not equal (as they will fail in performance code)
				assign("are_equal", 0.0);
				return;
			}
		}
		else
		{
			// If tables isn't in correct format, assumes HTFs are not equal (as they will fail in performance code)
			assign("are_equal", 0.0);
			return;
		}

		if( !htfProps1.equals(&htfProps2) )
		{
			assign("are_equal", 0.0);
			return;
		}
		else
			assign("are_equal", 1.0);	
	}

};

DEFINE_MODULE_ENTRY(user_htf_comparison, "Evaluates equivalence of two user-defined HTF tables", 0)