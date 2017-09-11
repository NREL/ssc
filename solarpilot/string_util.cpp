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

#include "string_util.h"
#include <stdio.h>
#include <stdlib.h>

using namespace std;



//---------------------------------------------
//  string and data handling
//---------------------------------------------
vector< string > split( const string &str, const string &delim, bool ret_empty, bool ret_delim )
{
	//Take a string with a delimiter and return a vector of separated values
	vector< string > list;

	char cur_delim[2] = {0,0};
	string::size_type m_pos = 0;
	string token;
	int dsize = (int)delim.size();
	
	while (m_pos < str.length())
	{
		//string::size_type pos = str.find_first_of(delim, m_pos);
		string::size_type pos = str.find(delim, m_pos);
		if (pos == string::npos)
		{
			cur_delim[0] = 0;
			token.assign(str, m_pos, string::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim[0] = str[pos];
			string::size_type len = pos - m_pos;			
			token.assign(str, m_pos, len);
			//m_pos = pos + 1;
			m_pos = pos + dsize;
		}
		
		if (token.empty() && !ret_empty)
			continue;

		list.push_back( token );
		
		if ( ret_delim && cur_delim[0] != 0 && m_pos < str.length() )
			list.push_back( string( cur_delim ) );
	}
	
	return list;
}

string join( const vector< string > &list, const string &delim )
{
	//Join a vector of strings separated by a delimiter
	string str;
	for (vector<string>::size_type i=0;i<list.size();i++)
	{
		str += list[i];
		if (i < list.size()-1)
			str += delim;
	}
	return str;		
}

bool to_integer(const string &str, int *x)
{
	//Convert a string value to an integer, assigning to x
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtol( startp, &endp, 10 );	
	return !*endp && (endp!=startp);
}

bool to_float(const string &str, float *x)
{
	double val;
	bool ok = to_double(str, &val);
	*x = (float) val;
	return ok;
}

bool to_double(const string &str, double *x)
{
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtod( startp, &endp );	
	return !*endp && (endp!=startp);
}

bool to_bool(const string &str, bool &x)
{
	bool val1 = false, val2 = false, val3 = false;
	string strl = lower_case(str);
	val1 = strl == "true";
	val2 = strl == "t";
	val3 = strl == "1";
	bool vals = (val1 || val2 || val3) == true;
	x = vals;
	return true;
}

string to_string( int x, const char *fmt )
{
	char buf[64];
	sprintf(buf, fmt, x);
	return string(buf);
}

string to_string( double x, const char *fmt )
{
	char buf[256];
	sprintf(buf, fmt, x);
	return string(buf);
}

string lower_case( const string &in )
{
	string ret(in);
	for (string::size_type i=0;i<ret.length();i++)
		ret[i] = (char)tolower(ret[i]);
	return ret;
}

string upper_case( const string &in )
{
	string ret(in);
	for (string::size_type i=0;i<ret.length();i++)
		ret[i] = (char)toupper(ret[i]);
	return ret;
}

string ReplaceString(string subject, const string& search, const string& replace) {
    size_t pos = 0;
    while ((pos = subject.find(search, pos)) != std::string::npos) {
         subject.replace(pos, search.length(), replace);
         pos += replace.length();
    }
    return subject;
}

void ReplaceStringInPlace(string& subject, const string& search, const string& replace) {
    size_t pos = 0;
    while ((pos = subject.find(search, pos)) != std::string::npos) {
         subject.replace(pos, search.length(), replace);
         pos += replace.length();
    }
}
