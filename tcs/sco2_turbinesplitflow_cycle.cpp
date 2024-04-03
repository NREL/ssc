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

#include "sco2_turbinesplitflow_cycle.h"
#include "sco2_cycle_components.h"

#include "CO2_properties.h"


#include "fmin.h"


// ********************************************************************************** C_sco2_htrbp_core CORE MODEL

void C_sco2_tsf_core::initialize_solve()
{
    m_outputs.Init();
}

int C_sco2_tsf_core::solve()
{
    return -1;
}

int C_sco2_tsf_core::finalize_design(C_sco2_cycle_core::S_design_solved& design_solved)
{
    // Design Main Compressor
    {
        int mc_design_err = m_outputs.m_mc_ms.design_given_outlet_state(m_inputs.m_mc_comp_model_code, m_outputs.m_temp[C_sco2_cycle_core::MC_IN],
            m_outputs.m_pres[C_sco2_cycle_core::MC_IN],
            m_outputs.m_m_dot_mc,
            m_outputs.m_temp[C_sco2_cycle_core::MC_OUT],
            m_outputs.m_pres[C_sco2_cycle_core::MC_OUT],
            m_inputs.m_des_tol);

        if (mc_design_err != 0)
        {
            m_outputs.m_error_code = mc_design_err;
            return m_outputs.m_error_code;
        }
    }

    // Design Recompressor
    if (m_inputs.m_recomp_frac > 0.01)
    {
        int rc_des_err = m_outputs.m_rc_ms.design_given_outlet_state(m_inputs.m_rc_comp_model_code, m_outputs.m_temp[C_sco2_cycle_core::LTR_LP_OUT],
            m_outputs.m_pres[C_sco2_cycle_core::LTR_LP_OUT],
            m_outputs.m_m_dot_rc,
            m_outputs.m_temp[C_sco2_cycle_core::RC_OUT],
            m_outputs.m_pres[C_sco2_cycle_core::RC_OUT],
            m_inputs.m_des_tol);

        if (rc_des_err != 0)
        {
            m_outputs.m_error_code = rc_des_err;
            return m_outputs.m_error_code;
        }

        design_solved.m_is_rc = true;
    }
    else
    {
        design_solved.m_is_rc = false;
    }

    // Size Turbine
    {
        C_turbine::S_design_parameters t_des_par;
        // Set turbine shaft speed
        t_des_par.m_N_design = m_inputs.m_N_turbine;
        t_des_par.m_N_comp_design_if_linked = m_outputs.m_mc_ms.get_design_solved()->m_N_design; //[rpm] m_mc.get_design_solved()->m_N_design;
        // Turbine inlet state
        t_des_par.m_P_in = m_outputs.m_pres[C_sco2_cycle_core::TURB_IN];
        t_des_par.m_T_in = m_outputs.m_temp[C_sco2_cycle_core::TURB_IN];
        t_des_par.m_D_in = m_outputs.m_dens[C_sco2_cycle_core::TURB_IN];
        t_des_par.m_h_in = m_outputs.m_enth[C_sco2_cycle_core::TURB_IN];
        t_des_par.m_s_in = m_outputs.m_entr[C_sco2_cycle_core::TURB_IN];
        // Turbine outlet state
        t_des_par.m_P_out = m_outputs.m_pres[C_sco2_cycle_core::TURB_OUT];
        t_des_par.m_h_out = m_outputs.m_enth[C_sco2_cycle_core::TURB_OUT];
        // Mass flow
        t_des_par.m_m_dot = m_outputs.m_m_dot_t;

        int turb_size_error_code = 0;
        m_outputs.m_t.turbine_sizing(t_des_par, turb_size_error_code);

        if (turb_size_error_code != 0)
        {
            m_outputs.m_error_code = turb_size_error_code;
            return m_outputs.m_error_code;
        }
    }

    // Design air cooler
    {
        // Structure for design parameters that are dependent on cycle design solution
        C_CO2_to_air_cooler::S_des_par_cycle_dep s_air_cooler_des_par_dep;
        // Set air cooler design parameters that are dependent on the cycle design solution
        s_air_cooler_des_par_dep.m_T_hot_in_des = m_outputs.m_temp[C_sco2_cycle_core::LTR_LP_OUT];  // [K]
        s_air_cooler_des_par_dep.m_P_hot_in_des = m_outputs.m_pres[C_sco2_cycle_core::LTR_LP_OUT];  // [kPa]
        s_air_cooler_des_par_dep.m_m_dot_total = m_outputs.m_m_dot_mc;                // [kg/s]

        // This pressure drop is currently uncoupled from the cycle design
        double cooler_deltaP = m_outputs.m_pres[C_sco2_cycle_core::LTR_LP_OUT] - m_outputs.m_pres[C_sco2_cycle_core::MC_IN];    // [kPa]
        if (cooler_deltaP == 0.0)
            s_air_cooler_des_par_dep.m_delta_P_des = m_inputs.m_deltaP_cooler_frac * m_outputs.m_pres[C_sco2_cycle_core::LTR_LP_OUT];    // [kPa]
        else
            s_air_cooler_des_par_dep.m_delta_P_des = cooler_deltaP; // [kPa]

        s_air_cooler_des_par_dep.m_T_hot_out_des = m_outputs.m_temp[C_sco2_cycle_core::MC_IN];                          // [K]
        s_air_cooler_des_par_dep.m_W_dot_fan_des = m_inputs.m_frac_fan_power * m_outputs.m_W_dot_net / 1000.0;     // [MWe]
        // Structure for design parameters that are independent of cycle design solution
        C_CO2_to_air_cooler::S_des_par_ind s_air_cooler_des_par_ind;
        s_air_cooler_des_par_ind.m_T_amb_des = m_inputs.m_T_amb_des;         // [K]
        s_air_cooler_des_par_ind.m_elev = m_inputs.m_elevation;              // [m]
        s_air_cooler_des_par_ind.m_eta_fan = m_inputs.m_eta_fan;             // [-]
        s_air_cooler_des_par_ind.m_N_nodes_pass = m_inputs.m_N_nodes_pass;   // [-]

        if (m_inputs.m_is_des_air_cooler && std::isfinite(m_inputs.m_deltaP_cooler_frac) && std::isfinite(m_inputs.m_frac_fan_power)
            && std::isfinite(m_inputs.m_T_amb_des) && std::isfinite(m_inputs.m_elevation) && std::isfinite(m_inputs.m_eta_fan) && m_inputs.m_N_nodes_pass > 0)
        {
            m_outputs.mc_air_cooler.design_hx(s_air_cooler_des_par_ind, s_air_cooler_des_par_dep, m_inputs.m_des_tol);
        }
    }

    // Get 'design_solved' structure from component classes
    design_solved.ms_mc_ms_des_solved = *m_outputs.m_mc_ms.get_design_solved();
    design_solved.ms_rc_ms_des_solved = *m_outputs.m_rc_ms.get_design_solved();
    design_solved.ms_t_des_solved = *m_outputs.m_t.get_design_solved();
    design_solved.ms_LTR_des_solved = m_outputs.mc_LT_recup.ms_des_solved;
    design_solved.ms_HTR_des_solved = m_outputs.mc_HT_recup.ms_des_solved;
    design_solved.ms_mc_air_cooler = *m_outputs.mc_air_cooler.get_design_solved();

    // Set solved design point metrics
    design_solved.m_temp = m_outputs.m_temp;
    design_solved.m_pres = m_outputs.m_pres;
    design_solved.m_enth = m_outputs.m_enth;
    design_solved.m_entr = m_outputs.m_entr;
    design_solved.m_dens = m_outputs.m_dens;

    design_solved.m_eta_thermal = m_outputs.m_eta_thermal;
    design_solved.m_W_dot_net = m_outputs.m_W_dot_net;
    design_solved.m_m_dot_mc = m_outputs.m_m_dot_mc;
    design_solved.m_m_dot_rc = m_outputs.m_m_dot_rc;
    design_solved.m_m_dot_t = m_outputs.m_m_dot_t;
    design_solved.m_recomp_frac = m_outputs.m_m_dot_rc / m_outputs.m_m_dot_t;
    design_solved.m_bypass_frac = m_inputs.m_bypass_frac;

    design_solved.m_UA_LTR = m_inputs.m_LTR_UA;
    design_solved.m_UA_HTR = m_inputs.m_HTR_UA;

    design_solved.m_W_dot_t = m_outputs.m_W_dot_t;		//[kWe]
    design_solved.m_W_dot_mc = m_outputs.m_W_dot_mc;		//[kWe]
    design_solved.m_W_dot_rc = m_outputs.m_W_dot_rc;		//[kWe]

    design_solved.m_W_dot_cooler_tot = m_outputs.mc_air_cooler.get_design_solved()->m_W_dot_fan * 1.E3;	//[kWe] convert from MWe

    return 0;
}

int C_sco2_tsf_core::solve_HTR_LTR(double T_HTR_HP_OUT_guess, double* diff_T_HTR_HP_out)
{
    return -1;
}

void C_sco2_tsf_core::reset()
{
    this->m_inputs = S_sco2_tsf_in();
    this->m_outputs.Init();
}

// ********************************************************************************** END C_sco2_htrbp_core


// ********************************************************************************** PRIVATE C_HTRBypass_Cycle (: C_sco2_cycle_core) 

/// <summary>
/// Core function to optimize cycle (fixed total UA)
/// </summary>
void C_TurbineSplitFlow_Cycle::auto_opt_design_core(int& error_code)
{
    // Create 'ms_opt_des_par' for Design Variables
    S_opt_design_parameters opt_par;
    {
        // Max Pressure
        double best_P_high = m_P_high_limit;		//[kPa]
        double PR_mc_guess = 2.5;				//[-]

        opt_par.m_fixed_P_mc_out = ms_auto_opt_des_par.m_fixed_P_mc_out;		//[-]
        if (!opt_par.m_fixed_P_mc_out)
        {
            double P_low_limit = std::min(m_P_high_limit, std::max(10.E3, m_P_high_limit * 0.2));		//[kPa]

            //best_P_high = fminbr(P_low_limit, m_P_high_limit, &fmin_cb_opt_des_fixed_P_high, this, 1.0);
            best_P_high = m_P_high_limit;
        }
        opt_par.m_P_mc_out_guess = best_P_high;      //[kPa]
        //ms_opt_des_par.m_fixed_P_mc_out = true;

        // Pressure Ratio (min pressure)
        opt_par.m_fixed_PR_HP_to_LP = ms_auto_opt_des_par.m_fixed_PR_HP_to_LP;			//[-]
        if (opt_par.m_fixed_PR_HP_to_LP)
        {
            opt_par.m_PR_HP_to_LP_guess = ms_auto_opt_des_par.m_PR_HP_to_LP_guess;	//[-]
        }
        else
        {
            opt_par.m_PR_HP_to_LP_guess = PR_mc_guess;		//[-]
        }

        // Is recompression fraction fixed or optimized?
        if (ms_auto_opt_des_par.m_is_turbinesplit_ok <= 0.0)
        {   // fixed
            opt_par.m_split_frac_guess = std::abs(ms_auto_opt_des_par.m_is_turbinesplit_ok);
            opt_par.m_fixed_split_frac = true;
        }
        else
        {   // optimized
            opt_par.m_split_frac_guess = 0.5;
            opt_par.m_fixed_split_frac = false;
        }

        // LTR HTR UA Ratio
        opt_par.m_LT_frac_guess = 0.5;
        opt_par.m_fixed_LT_frac = false;
        if (ms_auto_opt_des_par.m_LTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_auto_opt_des_par.m_HTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA)
        {
            opt_par.m_fixed_LT_frac = true;
        }

        // Set Design Method
        if (opt_par.m_fixed_LT_frac == true)
            opt_par.m_design_method = 3;
        else
            opt_par.m_design_method = 2;

    }

    // Set up baseline core inputs (will go somewhere else)
    C_sco2_tsf_core::S_sco2_tsf_in core_inputs;
    {
        // From Auto Opt Design Parameters
        core_inputs.m_LTR_target_code = ms_auto_opt_des_par.m_LTR_target_code;
        core_inputs.m_LTR_UA = ms_auto_opt_des_par.m_LTR_UA;
        core_inputs.m_LTR_min_dT = ms_auto_opt_des_par.m_LTR_min_dT;
        core_inputs.m_LTR_eff_target = ms_auto_opt_des_par.m_LTR_eff_target;
        core_inputs.m_LTR_eff_max = ms_auto_opt_des_par.m_LTR_eff_max;

        core_inputs.m_LTR_od_UA_target_type = ms_auto_opt_des_par.m_LTR_od_UA_target_type;
        core_inputs.m_HTR_target_code = ms_auto_opt_des_par.m_HTR_target_code;
        core_inputs.m_HTR_UA = ms_auto_opt_des_par.m_HTR_UA;
        core_inputs.m_HTR_min_dT = ms_auto_opt_des_par.m_HTR_min_dT;
        core_inputs.m_HTR_eff_target = ms_auto_opt_des_par.m_HTR_eff_target;
        core_inputs.m_HTR_eff_max = ms_auto_opt_des_par.m_HTR_eff_max;
        core_inputs.m_HTR_od_UA_target_type = ms_auto_opt_des_par.m_HTR_od_UA_target_type;
        core_inputs.m_des_tol = ms_auto_opt_des_par.m_des_tol;
        core_inputs.m_is_des_air_cooler = ms_auto_opt_des_par.m_is_des_air_cooler;

        // From Constructor
        core_inputs.m_LTR_N_sub_hxrs = m_LTR_N_sub_hxrs;            // Comes from constructor (constant)
        core_inputs.m_HTR_N_sub_hxrs = m_HTR_N_sub_hxrs;            // Comes from constructor (constant)
        core_inputs.m_W_dot_net_design = m_W_dot_net;               // Comes from constructor (constant)
        core_inputs.m_T_mc_in = m_T_mc_in;                          // Comes from constructor (constant)
        core_inputs.m_T_t_in = m_T_t_in;                            // Comes from constructor (constant)
        core_inputs.m_DP_LTR = m_DP_LTR;                            // Comes from constructor (constant)
        core_inputs.m_DP_HTR = m_DP_HTR;                            // Comes from constructor (constant)
        core_inputs.m_DP_PC_main = m_DP_PC_main;                    // Comes from constructor (constant)
        core_inputs.m_DP_PHX = m_DP_PHX;                            // Comes from constructor (constant)
        core_inputs.m_eta_mc = m_eta_mc;                            // Comes from constructor (constant)
        core_inputs.m_eta_t = m_eta_t;                              // Comes from constructor (constant)
        core_inputs.m_eta_t2 = m_eta_t2;                            // Comes from constructor (constant)
        core_inputs.m_eta_rc = m_eta_rc;                            // Comes from constructor (constant)
        core_inputs.m_eta_generator = m_eta_generator;              // Comes from constructor (constant)
        core_inputs.m_frac_fan_power = m_frac_fan_power;            // Comes from constructor (constant)
        core_inputs.m_eta_fan = m_eta_fan;                          // Comes from constructor (constant)
        core_inputs.m_deltaP_cooler_frac = m_deltaP_cooler_frac;    // Comes from constructor (constant)
        core_inputs.m_T_amb_des = m_T_amb_des;                      // Comes from constructor (constant)
        core_inputs.m_elevation = m_elevation;                      // Comes from constructor (constant)
        core_inputs.m_N_nodes_pass = m_N_nodes_pass;                // Comes from constructor (constant)
        core_inputs.m_mc_comp_model_code = m_mc_comp_model_code;    // Comes from constructor (constant)
        core_inputs.m_N_turbine = m_N_turbine;                      // Comes from constructor (constant)

        core_inputs.m_rc_comp_model_code = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby; // Constant


        // Handle design variables (check if fixed or free)
        {
            // Recompression Fraction
            if (opt_par.m_fixed_split_frac == true)
                core_inputs.m_recomp_frac = opt_par.m_split_frac_guess;
            else
                throw new C_csp_exception("not handled");

            // MC Outlet Pressure
            if (opt_par.m_fixed_P_mc_out == true)
                core_inputs.m_P_mc_out = opt_par.m_P_mc_out_guess;
            else
                throw new C_csp_exception("not handled");

            // Recuperator split fraction
            double LT_frac_local = opt_par.m_LT_frac_guess;
            if (ms_auto_opt_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_auto_opt_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA)
            {
                core_inputs.m_LTR_UA = ms_auto_opt_des_par.m_UA_rec_total * LT_frac_local;
                core_inputs.m_HTR_UA = ms_auto_opt_des_par.m_UA_rec_total * (1.0 - LT_frac_local);
            }
            else
            {
                core_inputs.m_LTR_UA = ms_auto_opt_des_par.m_LTR_UA;      //[kW/K]
                core_inputs.m_HTR_UA = ms_auto_opt_des_par.m_HTR_UA;      //[kW/K]
            }


            // Pressure Ratio is calculated in callback
        }

    }

    // Handle Pressure Ratio (temporary)
    double PR_mc_local = -999.9;
    double P_mc_in = -999.9;
    if (!opt_par.m_fixed_PR_HP_to_LP)
    {
        throw new C_csp_exception("not handled");
    }
    else
    {
        if (opt_par.m_PR_HP_to_LP_guess >= 0.0)
        {
            PR_mc_local = opt_par.m_PR_HP_to_LP_guess;
            P_mc_in = core_inputs.m_P_mc_out / PR_mc_local;		//[kPa]
        }
        else
        {
            P_mc_in = std::abs(opt_par.m_PR_HP_to_LP_guess);		//[kPa]
        }
    }

    core_inputs.m_P_mc_in = P_mc_in;


    // Should have enough to run model (with all variables fixed)
    C_sco2_tsf_core tsf_core;
    tsf_core.set_inputs(core_inputs);
    tsf_core.solve();

    return;
}

/// <summary>
/// Core function to optimize cycle for target eta (variable total UA)
/// </summary>
void C_TurbineSplitFlow_Cycle::auto_opt_design_hit_eta_core(int& error_code, const double eta_thermal_target)
{
    return;
}

/// <summary>
/// Optimize Total Recuperator UA
/// totalUA -> bp -> UA split, pressure, recomp
/// </summary>
/// <param name="auto_par"></param>
/// <param name="opt_par"></param>
/// <param name="optimal_inputs"></param>
/// <returns></returns>
int C_TurbineSplitFlow_Cycle::optimize_totalUA(const S_auto_opt_design_parameters& auto_par,
    const S_opt_design_parameters& opt_par,
    C_sco2_tsf_core::S_sco2_tsf_in& optimal_inputs)
{
    return -1;;
}

/// <summary>
/// Optimize internal variables (UA split, pressure, recomp)
/// totalUA -> bp -> UA split, pressure, recomp
/// </summary>
/// <param name="auto_par"></param>
/// <param name="opt_par"></param>
/// <param name="core_inputs"></param>
/// <param name="optimal_inputs"></param>
/// <returns></returns>
int C_TurbineSplitFlow_Cycle::optimize_par(const S_auto_opt_design_parameters& auto_par,
    const S_opt_design_parameters& opt_par,
    C_sco2_tsf_core::S_sco2_tsf_in& core_inputs,
    C_sco2_tsf_core::S_sco2_tsf_in& optimal_inputs)
{
    return -1;
}

// ********************************************************************************** PUBLIC Methods C_HTRBypass_Cycle (: C_sco2_cycle_core) 

/// <summary>
/// Optimize Cycle Design for FIXED total recuperator UA
/// </summary>
/// <returns></returns>
int C_TurbineSplitFlow_Cycle::auto_opt_design(S_auto_opt_design_parameters& auto_opt_des_par_in)
{
    // Reset Counter
    m_opt_iteration_count = 0;

    // Collect auto_opt_des parameters
    ms_auto_opt_des_par = auto_opt_des_par_in;

    int auto_opt_des_error_code = 0;

    // Design cycle
    auto_opt_design_core(auto_opt_des_error_code);

    return auto_opt_des_error_code;
}

/// <summary>
/// Optimize Cycle design to hit target eta (optimize total recuperator UA)
/// </summary>
/// <param name="auto_opt_des_hit_eta_in"></param>
/// <param name="error_msg"></param>
/// <returns></returns>
int C_TurbineSplitFlow_Cycle::auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters& auto_opt_des_hit_eta_in, std::string& error_msg)
{
    return -1;
}

// ********************************************************************************** PUBLIC Objective Functions (internal use only)

/// <summary>
/// Objective Function for total UA (outermost layer)
/// Total UA -> Bypass Fraction -> pressure, split UA, recomp frac
/// </summary>
/// <returns>ONLY Objective Value</returns>
double C_TurbineSplitFlow_Cycle::optimize_totalUA_return_objective_metric(const std::vector<double>& x,
    const S_auto_opt_design_parameters& auto_par,
    const S_opt_design_parameters& opt_par
)
{
    return 0;
}

/// <summary>
/// Objective Function for NON Bypass optimization
/// Total UA -> Bypass Fraction -> pressure, split UA, recomp frac
/// </summary>
/// <returns>ONLY Objective Value</returns>
double C_TurbineSplitFlow_Cycle::optimize_par_return_objective_metric(const std::vector<double>& x,
    const S_auto_opt_design_parameters& auto_par,
    const S_opt_design_parameters& opt_par,
    C_sco2_tsf_core& htrbp_core)
{
    return 0;
}

// ********************************************************************************** Off Design Functions


void C_TurbineSplitFlow_Cycle::reset_ms_od_turbo_bal_csp_solved()
{
}

int C_TurbineSplitFlow_Cycle::off_design_fix_shaft_speeds(S_od_par& od_phi_par_in, double od_tol)
{
    return 0;
}

int C_TurbineSplitFlow_Cycle::solve_OD_all_coolers_fan_power(double T_amb, double od_tol, double& W_dot_fan)
{
    return 0;
}

int C_TurbineSplitFlow_Cycle::solve_OD_mc_cooler_fan_power(double T_amb, double od_tol, double& W_dot_mc_cooler_fan, double& P_co2_out)
{
    return 0;
}

int C_TurbineSplitFlow_Cycle::solve_OD_pc_cooler_fan_power(double T_amb, double od_tol, double& W_dot_pc_cooler_fan, double& P_co2_out)
{
    return 0;
}

double C_TurbineSplitFlow_Cycle::get_od_temp(int n_state_point)
{
    return 0.0;
}

double C_TurbineSplitFlow_Cycle::get_od_pres(int n_state_point)
{
    return 0.0;
}

void C_TurbineSplitFlow_Cycle::check_od_solution(double& diff_m_dot, double& diff_E_cycle, double& diff_Q_LTR, double& diff_Q_HTR)
{
}

void C_TurbineSplitFlow_Cycle::set_od_temp(int n_state_point, double temp_K)
{
}

void C_TurbineSplitFlow_Cycle::set_od_pres(int n_state_point, double pres_kPa)
{
}

void C_TurbineSplitFlow_Cycle::off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, double tol, int& error_code, double& T_out)
{
}

void C_TurbineSplitFlow_Cycle::estimate_od_turbo_operation(double T_mc_in, double P_mc_in, double f_recomp, double T_t_in, double phi_mc, int& mc_error_code, double& mc_w_tip_ratio, double& P_mc_out, int& rc_error_code, double& rc_w_tip_ratio, double& rc_phi, bool is_update_ms_od_solved)
{
}


// ********************************************************************************** PUBLIC Methods defined outside of any class

double nlopt_tsf_optimize_totalUA_func(const std::vector<double>& x, std::vector<double>& grad, void* data)
{
    return 0;
}

double nlopt_tsf_optimize_par_func(const std::vector<double>& x, std::vector<double>& grad, void* data)
{
    return 0;
}

