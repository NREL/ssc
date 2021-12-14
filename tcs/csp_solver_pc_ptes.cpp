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

#include "csp_solver_pc_ptes.h"
#include "csp_solver_core.h"

// ***********************
// Inherited methods
// ***********************
C_pc_ptes::C_pc_ptes(double W_dot_out_des /*MWe*/, double eta_des /*-*/, double f_q_dot_cold_is_reject_des /*-*/,
    double T_HT_HTF_hot /*C*/, double T_HT_HTF_cold /*C*/, double T_CT_HTF_cold /*C*/, double T_CT_HTF_hot /*C*/,
    double cycle_max_frac /*-*/, double cycle_cutoff_frac /*-*/, double q_sby_frac /*-*/,
    double startup_time /*hr*/, double startup_frac /*hr*/,
    double htf_pump_coef /*kW/kg/s*/,
    int HT_htf_code /*-*/, util::matrix_t<double> HT_ud_htf_props,
    int CT_htf_code /*-*/, util::matrix_t<double> CT_ud_htf_props)
{

}

void C_pc_ptes::init(C_csp_power_cycle::S_solved_params& solved_params)
{
    throw(C_csp_exception("C_pc_tes::init() is not complete"));
}

C_csp_power_cycle::E_csp_power_cycle_modes C_pc_ptes::get_operating_state()
{
    throw(C_csp_exception("C_pc_tes::get_operating_state() is not complete"));
}

double C_pc_ptes::get_cold_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_cold_startup_time() is not complete"));
}

double C_pc_ptes::get_warm_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_warm_startup_time() is not complete"));
}

double C_pc_ptes::get_hot_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_hot_startup_time() is not complete"));
}

double C_pc_ptes::get_standby_energy_requirement()    //[MW]
{
    throw(C_csp_exception("C_pc_tes::get_standby_energy_requirement() is not complete"));
}

double C_pc_ptes::get_cold_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_cold_startup_energy() is not complete"));
}

double C_pc_ptes::get_warm_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_warm_startup_energy() is not complete"));
}

double C_pc_ptes::get_hot_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_hot_startup_energy() is not complete"));
}

double C_pc_ptes::get_max_thermal_power()     //MW
{
    throw(C_csp_exception("C_pc_tes::get_max_thermal_power() is not complete"));
}

double C_pc_ptes::get_min_thermal_power()     //MW
{
    throw(C_csp_exception("C_pc_tes::get_min_thermal_power() is not complete"));
}

void C_pc_ptes::get_max_power_output_operation_constraints(double T_amb /*C*/, double& m_dot_HTF_ND_max, double& W_dot_ND_max)	//[-] Normalized over design power
{
    throw(C_csp_exception("C_pc_tes::get_max_power_output_operation_constraints() is not complete"));
}

double C_pc_ptes::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double* w_dot_condenser)
{
    throw(C_csp_exception("C_pc_tes::get_efficiency_at_TPH() is not complete"));
}

double C_pc_ptes::get_efficiency_at_load(double load_frac, double* w_dot_condenser)
{
    throw(C_csp_exception("C_pc_tes::get_efficiency_at_load() is not complete"));
}

double C_pc_ptes::get_htf_pumping_parasitic_coef()		//[kWe/kWt]
{
    throw(C_csp_exception("C_pc_tes::get_htf_pumping_parasitic_coef() is not complete"));
}

// This can vary between timesteps for Type224, depending on remaining startup energy and time
double C_pc_ptes::get_max_q_pc_startup()		//[MWt]
{
    throw(C_csp_exception("C_pc_tes::get_max_q_pc_startup() is not complete"));
}


void C_pc_ptes::call(const C_csp_weatherreader::S_outputs& weather,
    C_csp_solver_htf_1state& htf_state_in,
    const C_csp_power_cycle::S_control_inputs& inputs,
    C_csp_power_cycle::S_csp_pc_out_solver& out_solver,
    //C_csp_power_cycle::S_csp_pc_out_report &out_report,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_pc_tes::call() is not complete"));
}

void C_pc_ptes::converged()
{
    throw(C_csp_exception("C_pc_tes::converged() is not complete"));
}

void C_pc_ptes::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_pc_tes::write_output_intervals() is not complete"));
}

void C_pc_ptes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
    throw(C_csp_exception("C_pc_tes::assign() is not complete"));
}
