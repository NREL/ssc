/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include <string>
#include <fstream>
#include "core.h"
#include "lib_util.h"
#include "lib_weatherfile.cpp"

static var_info _cm_wave_file_reader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                        UNITS     META                      GROUP                 REQUIRED_IF                CONSTRAINTS        UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "file_name",               "local weather file path",                     "",       "",                      "Weather Reader",      "*",                       "LOCAL_FILE",      "" },
	
// header data
	{ SSC_OUTPUT,        SSC_STRING,      "source",                  "Source",                                      "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "buoy_number",             "Buoy Number",                                 "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                                        "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                                       "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "country",                 "Country",                                     "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "distance_to_shore",       "Distance to shore",                           "m",      "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "depth",                  "Depth",                                        "m",      "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                    "Latitude",                                     "deg",    "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                    "Longitude",                                    "deg",    "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "tz",                    "Time zone",                                     "",       "",                      "Weather Reader",      "*",                        "",               "" },
																										            
																										            
// weather data records																					            
	{ SSC_OUTPUT,        SSC_MATRIX,      "freq_distribution",              "Frequency disttribution of resource",                                  "m/s",   "",                       "Weather Reader",      "*",                        "",                            "" },

var_info_invalid };

class cm_wave_file_reader : public compute_module
{
public:

	cm_wave_file_reader()
	{
		add_var_info(_cm_wave_file_reader);
	}
	
	void exec( )
	{	
		std::string file = as_string("file_name");

		if (file.empty())
		{
			throw exec_error("wave_file_reader", "File name missing.");
		}


		std::string buf, buf1;
		std::ifstream ifs(file);

		if (!ifs.is_open())
		{
			throw exec_error("wave_file_reader", "could not open file for reading: " + file);
		}

		getline(ifs, buf);
		getline(ifs, buf1);

		// header name value pairs
		std::vector<std::string> keys = split(buf);
		std::vector<std::string> values = split(buf1);
		int ncols = (int)keys.size();
		int ncols1 = (int)values.size();

		if (ncols != 10 || ncols1 != 10)
		{
			throw exec_error("wave_file_reader", "incorrect number of header columns: " + ncols );
		}

		assign("source", var_data(values[0]));
		assign("buoy_number", var_data(values[1]));
		assign("city", var_data(values[2]));
		assign("state", var_data(values[3]));
		assign("country", var_data(values[4]));
		assign("distance_to_shore", var_data(values[5]));
		assign("depth", var_data(values[6]));
		assign("lat", var_data(values[7]));
		assign("lon", var_data(values[8]));
		assign("tz", var_data(values[9]));

		// read in 21 rows x 22 columns
		util::matrix_t<double> mat = allocate_matrix("freq_distribution", 21, 22);
		for (size_t r = 0; r < 21; r++)
		{
			getline(ifs, buf);
			values.clear();
			values = split(buf);
			if (values.size() != 22)
			{
				throw exec_error("wave_file_reader", "incorrect number of data columns: " + values.size());
			}
			for (size_t c = 0; c < 22; c++)
			{
				if (r == 0 && c == 0)
					mat.at(r, c) = 0.0;
				else
					mat.at(r, c) = std::stod(values[c]);
			}
		}
	}
};

DEFINE_MODULE_ENTRY(wave_file_reader, "SAM Wave Resource File Reader", 1)
