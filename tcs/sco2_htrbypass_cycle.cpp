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

#include "sco2_htrbypass_cycle.h"
#include "sco2_cycle_components.h"

#include "CO2_properties.h"



void C_HTRBypass_Cycle::design_core(int& error_code)
{
    design_core_standard(error_code);
}

void C_HTRBypass_Cycle::design_core_standard(int& error_code)
{
    int x = 0;
}

void C_HTRBypass_Cycle::opt_design_core(int& error_code)
{
}

void C_HTRBypass_Cycle::auto_opt_design_core(int& error_code)
{
}

void C_HTRBypass_Cycle::finalize_design(int& error_code)
{
}

// Public Methods

void C_HTRBypass_Cycle::design(S_design_parameters& des_par_in, int& error_code)
{
    ms_des_par = des_par_in;

    int design_error_code = 0;

    design_core(design_error_code);

    if (design_error_code != 0)
    {
        error_code = design_error_code;
        return;
    }

    finalize_design(design_error_code);

    error_code = design_error_code;
}

void C_HTRBypass_Cycle::opt_design(S_opt_design_parameters& opt_des_par_in, int& error_code)
{
}

void C_HTRBypass_Cycle::reset_ms_od_turbo_bal_csp_solved()
{
}

int C_HTRBypass_Cycle::auto_opt_design(S_auto_opt_design_parameters& auto_opt_des_par_in)
{
    return 0;
}

int C_HTRBypass_Cycle::auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters& auto_opt_des_hit_eta_in, std::string& error_msg)
{
    return 0;
}

int C_HTRBypass_Cycle::off_design_fix_shaft_speeds(S_od_par& od_phi_par_in, double od_tol)
{
    return 0;
}

int C_HTRBypass_Cycle::solve_OD_all_coolers_fan_power(double T_amb, double od_tol, double& W_dot_fan)
{
    return 0;
}

int C_HTRBypass_Cycle::solve_OD_mc_cooler_fan_power(double T_amb, double od_tol, double& W_dot_mc_cooler_fan, double& P_co2_out)
{
    return 0;
}

int C_HTRBypass_Cycle::solve_OD_pc_cooler_fan_power(double T_amb, double od_tol, double& W_dot_pc_cooler_fan, double& P_co2_out)
{
    return 0;
}

double C_HTRBypass_Cycle::get_od_temp(int n_state_point)
{
    return 0.0;
}

double C_HTRBypass_Cycle::get_od_pres(int n_state_point)
{
    return 0.0;
}

void C_HTRBypass_Cycle::check_od_solution(double& diff_m_dot, double& diff_E_cycle, double& diff_Q_LTR, double& diff_Q_HTR)
{
}

void C_HTRBypass_Cycle::set_od_temp(int n_state_point, double temp_K)
{
}

void C_HTRBypass_Cycle::set_od_pres(int n_state_point, double pres_kPa)
{
}

void C_HTRBypass_Cycle::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, double tol, int& error_code, double& T_out)
{
}

void C_HTRBypass_Cycle::estimate_od_turbo_operation(double T_mc_in, double P_mc_in, double f_recomp, double T_t_in, double phi_mc, int& mc_error_code, double& mc_w_tip_ratio, double& P_mc_out, int& rc_error_code, double& rc_w_tip_ratio, double& rc_phi, bool is_update_ms_od_solved)
{
}
