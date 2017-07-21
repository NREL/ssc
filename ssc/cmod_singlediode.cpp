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
#include "lib_cec6par.h"

static var_info _cm_vtab_singlediode[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Vop",                     "Module operating voltage",       "V",      "",                      "Single Diode Model",      "?"                         "",                      "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "V",                       "Output voltage",                "V",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "I",                       "Output current",                "A",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Voc",                     "Open circuit voltage",          "V",      "",                      "Single Diode Model",       "*",                        "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Isc",                     "Short circuit current",         "A",      "",                      "Single Diode Model",       "*",                        "",                      "" },

var_info_invalid };


class cm_singlediode : public compute_module
{
private:
public:
	cm_singlediode()
	{
		add_var_info( _cm_vtab_singlediode );
	}

	void exec( ) throw( general_error )
	{
		double a = as_double( "a" );
		double Il = as_double( "Il" );
		double Io = as_double( "Io" );
		double Rs = as_double( "Rs" );
		double Rsh = as_double( "Rsh" );
		double Vop = -1.0;
		if ( is_assigned( "Vop" ) )
			Vop = as_double( "Vop" );

		double V, I;
		if ( Vop < 0 )
		{
			// use 100 volts as upper bound of Voc
			maxpower_5par( 100, a, Il, Io, Rs, Rsh, &V, &I );
		}
		else
		{
			V = Vop;
			I = current_5par( V, 0.9*Il, a, Il, Io, Rs, Rsh );
		}

		assign( "V", var_data( V ) );
		assign( "I", var_data( I ) );

		double Voc = openvoltage_5par( V, a, Il, Io, Rsh );
		double Isc = current_5par( 0.0, Il, a, Il, Io, Rs, Rsh );

		assign( "Voc", var_data( Voc ) );
		assign( "Isc", var_data( Isc ) );
	}
};

DEFINE_MODULE_ENTRY( singlediode, "Single diode model function.", 1 )


static var_info _cm_vtab_singlediodeparams[] = {

	/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	{ SSC_INPUT,        SSC_NUMBER,      "I",                       "Irradiance",                    "W/m2",      "",                    "Single Diode Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T",                       "Temperature",                   "C",         "",                    "Single Diode Model",      "*",                       "",              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "alpha_isc",               "Temp coeff of current at SC",    "A/'C",    "",                     "Single Diode Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Adj_ref",                 "OC SC temp coeff adjustment",    "%",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "a_ref",                   "Modified nonideality factor",    "1/V",     "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Il_ref",                  "Light current",                  "A",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Io_ref",                  "Saturation current",             "A",       "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rs_ref",                  "Series resistance",              "ohm",     "",                     "Single Diode Model",      "*",                        "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Rsh_ref",                 "Shunt resistance",               "ohm",     "",                     "Single Diode Model",      "*",                        "",                      "" },


	{ SSC_OUTPUT,       SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	
var_info_invalid };

class cm_singlediodeparams : public compute_module
{
private:
public:
	cm_singlediodeparams()
	{
		add_var_info( _cm_vtab_singlediodeparams );
	}
		
	void exec( ) throw( general_error )
	{
#define I_ref 1000.0
#define Tc_ref 298.15
#define Eg_ref 1.12
#define KB 8.618e-5
		
		double I = as_double( "I" );	
		double T = as_double( "T" ) + 273.15; // want cell temp in kelvin
		
		double alpha_isc = as_double( "alpha_isc" );
		double Adj = as_double( "Adj_ref" );
		double Il = as_double( "Il_ref" );
		double Io = as_double( "Io_ref" );
		double a = as_double( "a_ref" );
		double Rs = as_double( "Rs_ref" );
		double Rsh = as_double( "Rsh_ref" );

		
		double muIsc = alpha_isc * (1-Adj/100.0);
		// calculation of IL and IO at operating conditions
		double IL_oper = I/I_ref *( Il + muIsc*(T-Tc_ref) );
		if (IL_oper < 0.0) IL_oper = 0.0;
		
		double EG = Eg_ref * (1-0.0002677*(T-Tc_ref));
		double IO_oper = Io * pow(T/Tc_ref, 3) * exp( 1/KB*(Eg_ref/Tc_ref - EG/T) );
		double A_oper = a * T / Tc_ref;
		double Rsh_oper = Rsh*(I_ref/I);
			

		assign( "Rs", var_data(Rs) );
		assign( "Rsh", var_data( Rsh_oper ) );
		assign( "a", var_data(A_oper) );
		assign( "Io", var_data( IO_oper ) );
		assign( "Il", var_data( IL_oper ) );		
	}
};

DEFINE_MODULE_ENTRY( singlediodeparams, "Single diode model parameter calculation.", 1 )