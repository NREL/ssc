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
#include <fstream>
#include <future>

#include <json/json.h>

#include "sscapi.h"
#include "core.h"
#include "cmod_wind_landbosse.h"

static var_info _cm_vtab_wind_landbosse[] = {
     /*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        // Inputs
        { SSC_INPUT,        SSC_STRING, "wind_resource_filename",                "Local hourly wind data file path",                        "",         "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_rating_MW",                     "Turbine Rating",                                          "kW",       "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_turbine_rotor_diameter",           "Rotor Diameter",                                          "m",        "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_turbine_hub_ht",                   "Hub Height",                                              "m",        "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "num_turbines",                          "Number of Turbines",                                      "",         "",                      "LandBOSSE",      "*",                       "INTEGER,MIN=0",                 "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_resource_shear",                   "Wind Shear Exponent",                                     "",         "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_spacing_rotor_diameters",       "Turbine Spacing",                                         "diameters","",                      "LandBOSSE",      "*",                       "INTEGER,MIN=0",                 "" },
        { SSC_INPUT,        SSC_NUMBER, "row_spacing_rotor_diameters",           "Row Spacing",                                             "diameters","",                      "LandBOSSE",      "*",                       "INTEGER,MIN=0",                 "" },
        { SSC_INPUT,        SSC_NUMBER, "interconnect_voltage_kV",               "Interconnect Voltage",                                    "kV",       "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "distance_to_interconnect_mi",           "Distance to Interconnect",                                "miles",    "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "depth",                                 "Foundation Depth",                                        "m",        "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "rated_thrust_N",                        "Rated Thrust",                                            "N",        "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "labor_cost_multiplier",                 "Labor Cost Multiplier",                                   "",         "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "gust_velocity_m_per_s",                 "50 year Gust Velocity",                                   "m/s",      "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },

        { SSC_OUTPUT, 		SSC_STRING, "errors",	            		        "Error message",			                                 "",  "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "bonding_usd",			                "Management - Bonding Cost",		                       	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_equipment_rental_usd",       "Collection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_labor_usd",			        "Collection - Labor Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_material_usd",	            "Collection - Materials Cost",		                 	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_mobilization_usd",	        "Collection - Mobilization Cost",		                	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "construction_permitting_usd",	        "Management - Construction Permitting Cost",	    		 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_labor_usd",			        "Development - Labor Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_material_usd",		        "Development - Material Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_mobilization_usd",	        "Development - Mobilization Cost",		            	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "engineering_usd",			            "Management - Engineering Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_equipment_rental_usd",	        "Erection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_fuel_usd",			            "Erection - Fuel Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_labor_usd",		      	    "Erection - Labor Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_material_usd",		            "Erection - Material Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_mobilization_usd",		        "Erection - Mobilization Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_other_usd",			        "Erection - Other Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_equipment_rental_usd",       "Foundation - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_labor_usd",			        "Foundation - Labor Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_material_usd",		        "Foundation - Material Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_mobilization_usd",      	    "Foundation - Mobilization Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "insurance_usd",			                "Management - Insurance Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "markup_contingency_usd",	   	        "Management - Markup Contingency",			             "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "project_management_usd",		        "Management - Project Management Cost",			         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "site_facility_usd",			            "Management - Site Facility Cost",			             "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_equipment_rental_usd",  "Site Preparation - Equipment Rental Cost",		    	 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_labor_usd",			    "Site Preparation - Labor Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_material_usd",			"Site Preparation - Material Cost",			             "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_mobilization_usd",	    "Site Preparation - Mobilization Cost",			         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_collection_cost",			        "Total Collection Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_development_cost",		        "Total Development Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_erection_cost",			        "Total Erection Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_foundation_cost",			        "Total Foundation Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_gridconnection_cost",		   	    "Total Grid Connection Cost",			                     "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_management_cost",			        "Total Management Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_sitepreparation_cost",			"Total Site Preparation Cost",			                 "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_substation_cost",			        "Total Substation Cost",			                         "$", "", "LandBOSSE", "*", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_project_cost",			        "Total Project Cost",			                             "$", "", "LandBOSSE", "*", "", "", } ,

        var_info_invalid
};

cm_wind_landbosse::cm_wind_landbosse() {
    add_var_info(_cm_vtab_wind_landbosse);
    load_config();
}

void cm_wind_landbosse::load_config(){
    std::string python_config_path = get_python_path();

    if (python_config_path.empty())
        throw exec_error("wind_landbosse", "Path to SAM python configuration directory not set. "
                                           "Use 'set_python_path' function in sscapi.h to point to the correct folder.");

    // load python configuration
    Json::Value python_config_root;
    std::ifstream python_config_doc(python_config_path + "/python_config.json");
    if (python_config_doc.fail())
        throw exec_error("wind_landbosse", "Could not open 'python_config.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");

    python_config_doc >> python_config_root;

    if (!python_config_root.isMember("exec_path"))
        throw exec_error("wind_landbosse", "Missing key 'exec_path' in 'python_config.json'.");
    if (!python_config_root.isMember("python_version"))
        throw exec_error("wind_landbosse", "Missing key 'python_version' in 'python_config.json'.");


    python_exec_path = python_config_root["exec_path"].asString();
    auto python_version = python_config_root["python_version"].asString();

    // load landbosse configuration
    Json::Value landbosse_config_root;
    std::ifstream landbosse_config_doc(python_config_path + "/landbosse.json");
    if (landbosse_config_doc.fail())
        throw exec_error("wind_landbosse", "Could not open 'landbosse.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");

    landbosse_config_doc >> landbosse_config_root;

    if (!landbosse_config_root.isMember("run_cmd"))
        throw exec_error("wind_landbosse", "Missing key 'run_cmd' in 'landbosse.json'.");
    if (!landbosse_config_root.isMember("min_python_version"))
        throw exec_error("wind_landbosse", "Missing key 'min_python_version' in 'landbosse.json'.");

    python_run_cmd = landbosse_config_root["run_cmd"].asString();
    auto min_python_version = landbosse_config_root["min_python_version"].asString();

    // check version works out
    std::stringstream min_ver(min_python_version);
    std::stringstream py_ver(python_version);
    std::string min_ver_token, py_ver_token;

    while (std::getline(min_ver, min_ver_token, '.')) {
        if (!std::getline(py_ver, py_ver_token, '.'))
            return;
        if (std::stoi(min_ver_token) > std::stoi(py_ver_token))
            throw exec_error("wind_landbosse", "'min_python_version' requirement not met.");
    }
}

std::string cm_wind_landbosse::call_python_module(const std::string& input_json){
    std::promise<std::string> python_result;
    std::future<std::string> f_completes = python_result.get_future();
    std::thread([&](std::promise<std::string> python_result)
                {
                    std::string cmd = python_exec_path + " -c '" + python_run_cmd + "'";
                    size_t pos = cmd.find("<input>");
                    cmd.replace(pos, 7, input_json);

                    FILE *file_pipe = popen(cmd.c_str(), "r");
                    if (!file_pipe)
                        python_result.set_value_at_thread_exit("wind_landbosse error. Could not call python with cmd:\n" + cmd);

                    std::string mod_response;
                    char buffer[1024];
                    while (fgets(buffer, sizeof(buffer), file_pipe)){
                        mod_response += buffer;
                    }
                    pclose(file_pipe);
                    if (mod_response.empty())
                        python_result.set_value_at_thread_exit("LandBOSSE error. Function did not return a response.");
                    else
                        python_result.set_value_at_thread_exit(mod_response);
                },
                std::move(python_result)
    ).detach();

    std::chrono::system_clock::time_point time_passed
            = std::chrono::system_clock::now() + std::chrono::seconds(60 * 5);

    if(std::future_status::ready == f_completes.wait_until(time_passed))
        return f_completes.get();
    else
        throw exec_error("wind_landbosse", "python handler error. Python process timed out.");
}

void cm_wind_landbosse::exec() {
    m_vartab->rename_match_case("wind_resource_filename", "weather_file_path");
    m_vartab->rename_match_case("turbine_rating_mw", "turbine_rating_MW");
    m_vartab->rename_match_case("wind_turbine_rotor_diameter", "rotor_diameter_m");
    m_vartab->rename_match_case("wind_turbine_hub_ht", "hub_height_meters");
    m_vartab->rename_match_case("wind_resource_shear", "wind_shear_exponent");
    m_vartab->rename_match_case("interconnect_voltage_kv", "interconnect_voltage_kV");
    m_vartab->rename_match_case("rated_thrust_n", "rated_thrust_N");

    std::string input_json = ssc_data_to_json(m_vartab);
    std::string output_json = call_python_module(input_json);
    std::replace( output_json.begin(), output_json.end(), '\'', '\"');
    auto output_data = static_cast<var_table*>(json_to_ssc_data(output_json.c_str()));
    m_vartab->merge(*output_data, false);

    auto error_vd = m_vartab->lookup("errors");
    if (error_vd && error_vd->type == SSC_ARRAY)
        m_vartab->assign("errors", std::to_string(int(error_vd->num[0])));
    if (error_vd && error_vd->type == SSC_DATARR)
        m_vartab->assign("errors", error_vd->vec[0].str);
}

DEFINE_MODULE_ENTRY( wind_landbosse, "Land-based Balance-of-System Systems Engineering (LandBOSSE) cost model", 1 )
