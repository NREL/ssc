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
//#include "lib_weatherfile.h"
#include "lib_util.h"
#include "Toolbox.h"


static var_info _cm_vtab_layoutarea[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                          UNITS     META        GROUP          REQUIRED_IF         CONSTRAINTS         UI_HINTS*/

	
	
	{ SSC_INPUT,        SSC_MATRIX,      "positions",                 "Positions within calculataed area",          "",       "",         "layoutarea",   "*",                "",                "" },        
	
	/* outputs */
	{ SSC_OUTPUT,       SSC_MATRIX,      "convex_hull",               "Convex hull bounding the region",            "",       "",         "layoutarea",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "area",                      "Area inside the convex hull",                "",       "",         "layoutarea",   "*",                "",                "" },

	var_info_invalid };


#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

class cm_layoutarea : public compute_module
{
	/*struct Point
	{
		double x, y;
	};*/

public:
	
	cm_layoutarea()
	{
		add_var_info( _cm_vtab_layoutarea );
	}

	void exec( )
	{
		
		util::matrix_t<double> positions;
		//get the matrix of points
		get_matrix("positions", positions);
		//put into an array of points
		std::vector<sp_point> pos_pts;
		pos_pts.reserve( positions.nrows() );

		for(int i=0; i<(int)positions.nrows(); i++){
			pos_pts.push_back( sp_point () );
			pos_pts.back().x = positions.at(i, 0);
			pos_pts.back().y = positions.at(i, 1);
		}


		//Calculate the convex hull surrounding the heliostat positions
		std::vector<sp_point> hull;
		Toolbox::convex_hull(pos_pts, hull);

		//Calculate the area of the convex hull
		double area = Toolbox::area_polygon(hull);


		//return the results
		assign("area", (ssc_number_t)(area*0.000247105));	//acres
		ssc_number_t *hull_t = allocate( "convex_hull", hull.size(), 2);
		for(int i=0; i<(int)hull.size(); i++){
			hull_t[i * 2] = (ssc_number_t)hull.at(i).x;
			hull_t[i * 2 + 1] = (ssc_number_t)hull.at(i).y;
		}
		

	}
};


DEFINE_MODULE_ENTRY( layoutarea, "Layout Area Calculation", 0 )
