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

#include "csp_solver_cr_heat_pump.h"
#include "csp_solver_core.h"

C_csp_cr_heat_pump::C_csp_cr_heat_pump(double COP_heat_des /*-*/, double q_dot_hot_out_des /*MWt*/,
    double T_HT_HTF_hot /*C*/, double T_HT_HTF_cold /*C*/, double T_CT_HTF_cold /*C*/, double T_CT_HTF_hot /*C*/,
    double f_q_dot_min /*-*/, double f_q_dot_des_allowable_su /*-*/, double hrs_startup_at_max_rate /*hr*/,
    int HT_htf_code /*-*/, util::matrix_t<double> HT_ud_htf_props,
    int CT_htf_code /*-*/, util::matrix_t<double> CT_ud_htf_props)
{
    m_COP_heat_des = COP_heat_des;              //[-]
    m_q_dot_hot_out_des = q_dot_hot_out_des;    //[MWt]
    m_T_HT_HTF_hot = T_HT_HTF_hot;              //[C]
    m_T_HT_HTF_cold = T_HT_HTF_cold;            //[C]
    m_T_CT_HTF_cold = T_CT_HTF_cold;            //[C]
    m_T_CT_HTF_hot = T_CT_HTF_hot;              //[C]

    m_q_dot_min = f_q_dot_min*m_q_dot_hot_out_des;  //[MWt]

    m_f_q_dot_des_allowable_su = f_q_dot_des_allowable_su;  //[-]
    m_hrs_startup_at_max_rate = hrs_startup_at_max_rate;    //[hr]

    m_HT_htf_code = HT_htf_code;
    m_HT_ud_htf_props = HT_ud_htf_props;

    m_CT_htf_code = CT_htf_code;
    m_CT_ud_htf_props = CT_ud_htf_props;
}

C_csp_cr_heat_pump::~C_csp_cr_heat_pump(){}

// ***********************
// Inherited methods
// ***********************
void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
    C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::init() is not complete"));
}

C_csp_collector_receiver::E_csp_cr_modes get_operating_state()
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_operating_state() is not complete"));
}

double get_startup_time()
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_startup_time() is not complete"));
}

double get_startup_energy() //MWh
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_startup_energy() is not complete"));
}

double get_pumping_parasitic_coef()  //MWe/MWt
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_pumping_parasitic_coef() is not complete"));
}

double get_min_power_delivery()    //MWt
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_min_power_delivery() is not complete"));
}

double get_max_power_delivery(double T_cold_in)   //MWt
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_max_power_delivery() is not complete"));
}

double get_tracking_power()		//MWe
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_tracking_power() is not complete"));
}

double get_col_startup_power()		//MWe-hr
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_col_startup_power() is not complete"));
}

void off(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::off() is not complete"));
}

void startup(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::startup() is not complete"));
}

void on(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double q_dot_elec_to_CR_heat /*MWt*/, double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::on() is not complete"));
}

void estimates(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_est_out& est_out,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::estimates() is not complete"));
}

void converged()
{
    throw(C_csp_exception("C_csp_cr_heat_pump::converged() is not complete"));
}

void write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::write_output_intervals() is not complete"));
}

double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs& weather, const C_csp_solver_sim_info& sim)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::calculate_optical_efficiency() is not complete"));
}

double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs& weather, double q_incident /*MW*/)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::calculate_thermal_efficiency() is not complete"));
}

double get_collector_area()
{
    // Collector area is not a relevant metric for a heat pump
    return std::numeric_limits<double>::quiet_NaN();
}

// ***************************************************************
// ***************************************************************
