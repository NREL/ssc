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

#include <json/json.h>

#include "sscapi.h"
#include "core.h"
#include "cmod_wind_landbosse.h"

static var_info _cm_vtab_wind_landbosse[] = {
     /*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        // Inputs
        { SSC_INPUT,        SSC_STRING, "wind_resource_filename",                "Local wind data file path",                               "",         "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_rating",                        "Turbine Rating",                                          "kW",       "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "rotor_diameter",                        "Rotor Diameter",                                          "m",        "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "hub_height",                            "Hub Height",                                              "m",        "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "number_of_turbines",                    "Number of Turbines",                                      "",         "",                      "LandBOSSE",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER, "interconnect_voltage",                  "Interconnect Voltage",                                    "kV",       "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "distance_to_interconnect",              "Distance to Interconnect",                                "miles",    "",                      "LandBOSSE",      "*",                       "",                              "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_spacing",                       "Turbine Spacing",                                         "diameters","",                      "LandBOSSE",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER, "row_spacing",                           "Row Spacing",                                             "diameters","",                      "LandBOSSE",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER, "soil_condition",                        "Soil Condition",                                          "",         "",                      "LandBOSSE",      "*",                       "INTEGER",                       "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_resource_shear",                   "Shear exponent",                                          "",         "",                      "LandBOSSE",      "*",                       "MIN=0",                         "" },

        { SSC_OUTPUT, 		SSC_STRING, "errors",	            		        "Error message",			                                 "",  "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "bonding_usd",			                "Management - Bonding Cost",		                       	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_equipment_rental_usd",       "Collection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_labor_usd",			        "Collection - Labor Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_material_usd",	            "Collection - Materials Cost",		                 	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_mobilization_usd",	        "Collection - Mobilization Cost",		                	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "construction_permitting_usd",	        "Management - Construction Permitting Cost",	    		 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_labor_usd",			        "Development - Labor Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_material_usd",		        "Development - Material Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_mobilization_usd",	        "Development - Mobilization Cost",		            	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "engineering_usd",			            "Management - Engineering Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_equipment_rental_usd",	        "Erection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_fuel_usd",			            "Erection - Fuel Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_labor_usd",		      	    "Erection - Labor Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_material_usd",		            "Erection - Material Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_mobilization_usd",		        "Erection - Mobilization Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_other_usd",			        "Erection - Other Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_equipment_rental_usd",       "Foundation - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_labor_usd",			        "Foundation - Labor Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_material_usd",		        "Foundation - Material Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_mobilization_usd",      	    "Foundation - Mobilization Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "insurance_usd",			                "Management - Insurance Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "markup_contingency_usd",	   	        "Management - Markup Contingency",			             "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "project_management_usd",		        "Management - Project Management Cost",			         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "site_facility_usd",			            "Management - Site Facility Cost",			             "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_equipment_rental_usd",  "Site Preparation - Equipment Rental Cost",		    	 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_labor_usd",			    "Site Preparation - Labor Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_material_usd",			"Site Preparation - Material Cost",			             "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_mobilization_usd",	    "Site Preparation - Mobilization Cost",			         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_collection_cost",			        "Total Collection Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_development_cost",		        "Total Development Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_erection_cost",			        "Total Erection Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_foundation_cost",			        "Total Foundation Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_gridconnection_cost",		   	    "Total Grid Connection Cost",			                     "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_management_cost",			        "Total Management Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_sitepreparation_cost",			"Total Site Preparation Cost",			                 "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_substation_cost",			        "Total Substation Cost",			                         "$", "", "LandBOSSE", "", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_project_cost",			        "Total Project Cost",			                             "$", "", "LandBOSSE", "", "", "", } ,

        var_info_invalid
};

cm_wind_landbosse::cm_wind_landbosse() {
    add_var_info(_cm_vtab_wind_landbosse);
    load_config();
}

void cm_wind_landbosse::load_config(){
    std::string python_config_path = get_python_path();

    if (python_exec_path.empty()){
        throw exec_error("wind_landbosse", "Path to SAM python configuration directory not set. "
                                           "Use 'set_python_path' function in sscapi.h to point to the correct folder.");
    }

    std::ifstream python_config_doc(python_config_path + "/python_config.json", std::ifstream::binary);
    if (!python_config_doc)
        throw exec_error("wind_landbosse", "Could not open 'python_config.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");
    Json::Value python_config_root;
    python_config_doc >> python_config_root;

    if (!python_config_root.isMember("exec_path"))
        throw exec_error("wind_landbosse", "Missing key 'exec_path' in 'python_config.json'.");
    python_exec_path = python_config_root["exec_path"].asString();

    if (!python_config_root.isMember("python_version"))
        throw exec_error("wind_landbosse", "Missing key 'python_version' in 'python_config.json'.");
    auto python_version = python_config_root["python_version"].asString();

    std::ifstream landbosse_config_doc(python_config_path + "/landbosse.json", std::ifstream::binary);
    if (!landbosse_config_doc)
        throw exec_error("wind_landbosse", "Could not open 'landbosse.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");
    Json::Value landbosse_config_root;
    landbosse_config_doc >> landbosse_config_root;

    if (!landbosse_config_root.isMember("run_cmd"))
        throw exec_error("wind_landbosse", "Missing key 'run_cmd' in 'landbosse.json'.");
    python_run_cmd = landbosse_config_root["run_cmd"].asString();

    if (!python_config_root.isMember("min_python_version"))
        throw exec_error("wind_landbosse", "Missing key 'min_python_version' in 'landbosse.json'.");
    auto min_python_version = landbosse_config_root["min_python_version"].asString();

    // check version works out
}

std::string cm_wind_landbosse::call_python_module(const std::string& input_json){
    std::promise<std::string> python_result;
    std::future<std::string> f_completes = python_result.get_future();
    std::thread([&](std::promise<std::string> python_result)
                {
                    std::string cmd = python_exec_path + " -c " + python_run_cmd;
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

    std::chrono::system_clock::time_point two_seconds_passed
            = std::chrono::system_clock::now() + std::chrono::seconds(60);

    if(std::future_status::ready == f_completes.wait_until(two_seconds_passed)){
        std::cout << f_completes.get() << "\n"; }
    else{
        std::cout << "python handler error. Python process timed out.\n"; }
}

void cm_wind_landbosse::exec() {
    std::string input_json = ssc_data_to_json(m_vartab);
    std::string output_json = call_python_module(input_json);
    m_vartab = static_cast<var_table*>(json_to_ssc_data(output_json.c_str()));
}

DEFINE_MODULE_ENTRY( wind_landbosse, "Land-based Balance-of-System Systems Engineering (LandBOSSE) cost model", 1 )
