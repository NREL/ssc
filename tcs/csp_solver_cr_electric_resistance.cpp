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

#include "csp_solver_cr_electric_resistance.h"
#include "csp_solver_core.h"

C_csp_cr_electric_resistance::C_csp_cr_electric_resistance()
{}

C_csp_cr_electric_resistance::~C_csp_cr_electric_resistance()
{}

// Inherited methods
void C_csp_cr_electric_resistance::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
    C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::init(...) is not complete"));
}

int C_csp_cr_electric_resistance::get_operating_state()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_operating_state(...) is not complete"));
    return -1;
}

double C_csp_cr_electric_resistance::get_startup_time()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_startup_time(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_startup_energy() //MWh
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_startup_energy(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_pumping_parasitic_coef()  //MWe/MWt
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_pumping_parasitic_coef(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_min_power_delivery()    //MWt
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_min_power_delivery(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_tracking_power()	//MWe
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_tracking_power(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_col_startup_power()		//MWe-hr
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_col_startup_power(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_cr_electric_resistance::off(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::off(...) is not complete"));
}

void C_csp_cr_electric_resistance::startup(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::startup(...) is not complete"));
}

void C_csp_cr_electric_resistance::on(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::on(...) is not complete"));
}

void C_csp_cr_electric_resistance::estimates(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_est_out& est_out,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::estimates(...) is not complete"));
}

void C_csp_cr_electric_resistance::converged()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::converged(...) is not complete"));
}

void C_csp_cr_electric_resistance::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::write_output_intervals(...) is not complete"));
}

double C_csp_cr_electric_resistance::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs& weather, const C_csp_solver_sim_info& sim)
{
    // Optical efficiency is not a relevant metric for electric resistance heater
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs& weather, double q_incident /*MW*/)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::calculate_thermal_efficiency_approx(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_collector_area()
{
    // Used by dispatch model
    return 0.0;
}
