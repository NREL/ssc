/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include "csp_solver_NTHeatTrap_tes.h"

C_csp_NTHeatTrap_tes::C_csp_NTHeatTrap_tes()
{
}

void C_csp_NTHeatTrap_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
    double x = 0;
}

bool C_csp_NTHeatTrap_tes::does_tes_exist()
{
	return false;
}

bool C_csp_NTHeatTrap_tes::is_cr_to_cold_allowed()
{
	return false;
}

double C_csp_NTHeatTrap_tes::get_hot_temp()
{
	return 0;	//[K]
}

double C_csp_NTHeatTrap_tes::get_cold_temp()
{
	return 0;	//[K]
}

double C_csp_NTHeatTrap_tes::get_hot_tank_vol_frac()
{
	return 0;
}

double C_csp_NTHeatTrap_tes::get_initial_charge_energy()
{
    return 0;
}

double C_csp_NTHeatTrap_tes::get_min_charge_energy()
{
    //MWh
    return 0.; //m_q_pb_design * m_ts_hours * m_h_tank_min / m_h_tank*1.e-6;
}

double C_csp_NTHeatTrap_tes::get_max_charge_energy()
{
    return 0;
}

double C_csp_NTHeatTrap_tes::get_degradation_rate()
{
    return 0;
}

void C_csp_NTHeatTrap_tes::reset_storage_to_initial_state()
{
}

void C_csp_NTHeatTrap_tes::discharge_avail_est(double T_cold_K, double step_s,
    double& q_dot_dc_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_hot_external_est /*K*/)
{
}

void C_csp_NTHeatTrap_tes::charge_avail_est(double T_hot_K, double step_s,
    double& q_dot_ch_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_cold_external_est /*K*/)
{
}

int C_csp_NTHeatTrap_tes::solve_tes_off_design(double timestep /*s*/, double  T_amb /*K*/,
    double m_dot_cr_to_cv_hot /*kg/s*/, double m_dot_cv_hot_to_sink /*kg/s*/, double m_dot_cr_to_cv_cold /*kg/s*/,
    double T_cr_out_hot /*K*/, double T_sink_out_cold /*K*/,
    double& T_sink_htf_in_hot /*K*/, double& T_cr_in_cold /*K*/,
    C_csp_tes::S_csp_tes_outputs& s_outputs)		//, C_csp_solver_htf_state & s_tes_ch_htf, C_csp_solver_htf_state & s_tes_dc_htf)
{
    return 0;
}

void C_csp_NTHeatTrap_tes::converged()
{
}

void C_csp_NTHeatTrap_tes::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
}

void C_csp_NTHeatTrap_tes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
}

double /*MWe*/ C_csp_NTHeatTrap_tes::pumping_power(double m_dot_sf /*kg/s*/, double m_dot_pb /*kg/s*/, double m_dot_tank /*kg/s*/,
    double T_sf_in /*K*/, double T_sf_out /*K*/, double T_pb_in /*K*/, double T_pb_out /*K*/, bool recirculating)
{
    return 0;
}