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
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include "base_dispatch.h"
//#include "lp_lib.h" 
//#include "lib_util.h"

/*

Careful with namespaces in this file.. importing the LPsolve library introduces new macro definitions
and function definitions.

    #define SOS_NONE
    //#define SOS_SEQUENCE
    //#define SOS_MANUAL
    //#define SOS_LPSOLVE
*/


base_dispatch_opt::base_dispatch_opt()
{
    //initialize member data
    m_nstep_opt = 0;
    m_is_weather_setup = false;
    
    clear_output();
}

void base_dispatch_opt::not_implemented_function(std::string function_name)
{
    throw std::runtime_error(function_name + " is not implemented.");
}

void base_dispatch_opt::clear_output()
{
    m_current_read_step = 0;
    lp_outputs.clear_output();
}

void base_dispatch_opt::init(double cycle_q_dot_des, double cycle_eta_des, double cycle_w_dot_des)
{
    not_implemented_function((std::string)__func__);
}

bool base_dispatch_opt::check_setup()
{
    //check parameters and inputs to make sure everything has been set up correctly
    if( !m_is_weather_setup ) return false;
    if( !pointers.siminfo ) return false;
    
    return true;
}

//bool base_dispatch_opt::copy_weather_data(C_csp_weatherreader *weather_source)
//{
//    //Copy the weather data
//    m_weather = weather_source;
//    m_is_weather_setup = true;
//
//    return m_is_weather_setup;
//}

bool base_dispatch_opt::update_horizon_parameters(C_csp_tou &mc_tou)
{
    not_implemented_function((std::string)__func__);
    return false;
}

void base_dispatch_opt::update_initial_conditions(double q_dot_to_pb, double T_htf_cold_des)
{
    not_implemented_function((std::string)__func__);
}

bool base_dispatch_opt::predict_performance(int step_start, int ntimeints, int divs_per_int)
{
    not_implemented_function((std::string)__func__);
    return false;
}

bool base_dispatch_opt::optimize()
{
    not_implemented_function((std::string)__func__);
    return false;
}

std::string base_dispatch_opt::write_ampl()
{
    not_implemented_function((std::string)__func__);
    return "";
}

bool base_dispatch_opt::optimize_ampl()
{
    not_implemented_function((std::string)__func__);
    return false;
}

bool base_dispatch_opt::set_dispatch_outputs()
{
    not_implemented_function((std::string)__func__);
    return false;
}

bool base_dispatch_opt::strcompare(std::string a, std::string b)
{
    return util::lower_case(a) < util::lower_case(b);
}
