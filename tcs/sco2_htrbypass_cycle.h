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

#ifndef __SCO2_HTRBYPASS_
#define __SCO2_HTRBYPASS_

#include "sco2_cycle_components.h"
#include "sco2_cycle_templates.h"

#include "heat_exchangers.h"
#include "CO2_properties.h"


class C_HTRBypass_Cycle : public C_sco2_cycle_core
{
public:

    enum E_htrbypass_cycle_state_points
    {
        // index values for c++ 0-based vectors for temperature, pressure, etc.
        MC_IN = 0,		// Main compressor inlet
        MC_OUT,			// Main compressor outlet
        LTR_HP_OUT,		// Low temp recuperator high pressure outlet
        MIXER_OUT,		// Mixer: LTR_HP_OUT + Recompressor outlet
        HTR_HP_OUT,		// High temp recuperator high pressure outlet
        TURB_IN,		// Turbine inlet
        TURB_OUT,		// Turbine outlet
        HTR_LP_OUT,		// High temp recuperator low pressure outlet
        LTR_LP_OUT,		// Low temp recuperator low pressure outlet
        RC_OUT,			// Recompresor outlet
        BYPASS_OUT,
        MIXER2_OUT,


        END_SCO2_HTRBP_STATES
    };

    struct S_design_parameters
    {
        // Parameters SHARED with recompression

        double m_P_mc_in;					//[kPa] Compressor inlet pressure
        double m_P_mc_out;					//[kPa] Compressor outlet pressure

        // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
        double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;

        // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;

        double m_recomp_frac;				//[-] Fraction of flow that bypasses the precooler and the main compressor at the design point
        double m_des_tol;						//[-] Convergence tolerance

        // Air cooler parameters
        bool m_is_des_air_cooler;		//[-] False will skip physical air cooler design. UA will not be available for cost models.

        int m_des_objective_type;		//[2] = min phx deltat then max eta, [else] max eta
        double m_min_phx_deltaT;		//[C]


        S_design_parameters()
        {
            m_P_mc_in = m_P_mc_out =
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max =
                m_recomp_frac =
                m_des_tol =
                std::numeric_limits<double>::quiet_NaN();

            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

            // Default to standard optimization to maximize cycle efficiency
            m_des_objective_type = 1;
            m_min_phx_deltaT = 0.0;		//[C]

            // Air cooler default
            m_is_des_air_cooler = true;

        }


    };

    struct S_opt_design_parameters
    {
        double m_UA_rec_total;				//[kW/K] Total design-point recuperator UA
        // LTR thermal design
        int m_LTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_LTR_UA;					//[kW/K] target LTR conductance
        double m_LTR_min_dT;                //[K] target LTR minimum temperature difference
        double m_LTR_eff_target;            //[-] target LTR effectiveness
        double m_LTR_eff_max;				//[-] Maximum allowable effectiveness in LT recuperator
        NS_HX_counterflow_eqs::E_UA_target_type m_LTR_od_UA_target_type;
        // HTR thermal design
        int m_HTR_target_code;              //[-] 1 = UA, 2 = min dT, 3 = effectiveness
        double m_HTR_UA;					//[kW/K] target HTR conductance
        double m_HTR_min_dT;                //[K] target HTR min temperature difference
        double m_HTR_eff_target;            //[-] target HTR effectiveness
        double m_HTR_eff_max;				//[-] Maximum allowable effectiveness in HT recuperator
        NS_HX_counterflow_eqs::E_UA_target_type m_HTR_od_UA_target_type;
        //
        double m_des_tol;					//[-] Convergence tolerance
        double m_des_opt_tol;					//[-] Optimization tolerance

        // Air cooler parameters
        bool m_is_des_air_cooler;		//[-] False will skip physical air cooler design. UA will not be available for cost models.

        int m_des_objective_type;			//[2] = min phx deltat then max eta, [else] max eta
        double m_min_phx_deltaT;			//[C]

        double m_P_mc_out_guess;			//[kPa] Initial guess for main compressor outlet pressure
        bool m_fixed_P_mc_out;				//[-] if true, P_mc_out is fixed at P_mc_out_guess

        double m_PR_HP_to_LP_guess;         //[-] Initial guess for ratio of P_mc_out to P_LP_in
        bool m_fixed_PR_HP_to_LP;					//[-] if true, ratio of P_mc_out to P_mc_in is fixed at PR_mc_guess

        double m_recomp_frac_guess;			//[-] Initial guess for design-point recompression fraction
        bool m_fixed_recomp_frac;			//[-] if true, recomp_frac is fixed at recomp_frac_guess

        double m_LT_frac_guess;				//[-] Initial guess for fraction of UA_rec_total that is in the low-temperature recuperator
        bool m_fixed_LT_frac;				//[-] if true, LT_frac is fixed at LT_frac_guess

        S_opt_design_parameters()
        {
            m_UA_rec_total =
                m_LTR_UA = m_LTR_min_dT = m_LTR_eff_target = m_LTR_eff_max =
                m_HTR_UA = m_HTR_min_dT = m_HTR_eff_target = m_HTR_eff_max =
                m_des_tol = m_des_opt_tol =
                m_P_mc_out_guess = m_PR_HP_to_LP_guess = m_recomp_frac_guess = m_LT_frac_guess =
                std::numeric_limits<double>::quiet_NaN();

            // Recuperator design target codes
            m_LTR_target_code = 1;      // default to target conductance
            m_LTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;
            m_HTR_target_code = 1;      // default to target conductance
            m_HTR_od_UA_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_calc_UA;

            // Air cooler default
            m_is_des_air_cooler = true;

            // Default to standard optimization to maximize cycle efficiency
            m_des_objective_type = 1;
            m_min_phx_deltaT = 0.0;		//[C]

        }


    };

private:

    // Component classes
    C_turbine m_t;
    C_comp_multi_stage m_mc_ms;
    C_comp_multi_stage m_rc_ms;
    C_HeatExchanger m_PHX, m_PC;

    C_HX_co2_to_co2_CRM mc_LT_recup;
    C_HX_co2_to_co2_CRM mc_HT_recup;

    C_CO2_to_air_cooler mc_air_cooler;

    // Input/Ouput structures for class methods
    S_design_parameters ms_des_par;
    S_opt_design_parameters ms_opt_des_par;

    // Results from last 'design' solution
    std::vector<double> m_temp_last, m_pres_last, m_enth_last, m_entr_last, m_dens_last;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
    double m_eta_thermal_calc_last;
    double m_W_dot_net_last;
    double m_m_dot_mc, m_m_dot_rc, m_m_dot_t;
    double m_Q_dot_PHX, m_Q_dot_bypass, m_eta_bypass;
    double m_W_dot_mc, m_W_dot_rc, m_W_dot_t, m_W_dot_mc_bypass;
    double m_objective_metric_last;

    // Structures and data for optimization
    S_design_parameters ms_des_par_optimal;
    double m_objective_metric_opt;

    // Structures and data for auto-optimization
    double m_objective_metric_auto_opt;
    S_design_parameters ms_des_par_auto_opt;

    // NEW Internal Variables
    double m_w_t;
    double m_w_mc;
    double m_w_rc;
    double m_Q_dot_LT, m_Q_dot_HT;
    double m_bp_frac;
    double m_m_dot_bp;
    double m_m_dot_htr_hp;
    double m_cp_HTF;
    double m_m_dot_HTF;
    double m_Q_dot_BP;
    double m_T_HTF_PHX_inlet;
    double m_T_HTF_BP_outlet;
    double m_T_HTF_PHX_out;
    double m_dT_BP; // BYPASS_OUT - HTR_HP_OUT
    double m_Q_dot_total;
    double m_Q_dot_pc;  // pre cooler heat rejected
    double m_HTF_BP_cold_approach;
    double m_HTF_PHX_cold_approach;

    C_HX_co2_to_co2_CRM m_BP_HTX;

    // New opt
    bool m_found_opt;
    double m_eta_phx_max;
    double m_UA_diff_eta_max;
    double m_over_deltaP_eta_max;

    void design_core(int& error_code);

    void design_core_standard(int& error_code);

    void opt_design_core(int& error_code);

    void auto_opt_design_core(int& error_code);

    void finalize_design(int& error_code);


    // Added
    int solve_cycle(double bypass_fraction, double& error);
    int solve_HTR(double T_HTR_LP_OUT_guess, double& T_HTR_LP_out_calc);
    int solve_LTR(double T_LTR_LP_OUT_guess, double& T_LTR_LP_out_calc);
    int solve_bypass(double T_BP_OUT_guess, double& T_BP_out_calc);
    int solve_bypass_energy(double T_BP_OUT_guess, double& T_BP_out_calc);

public:

    C_HTRBypass_Cycle(C_sco2_cycle_core::E_turbo_gen_motor_config turbo_gen_motor_config,
        double eta_generator,
        double T_mc_in,
        double W_dot_net,
        double T_t_in, double P_high_limit,
        std::vector<double> DP_LTR, std::vector<double> DP_HTR,
        std::vector<double> DP_PC_main, std::vector<double> DP_PHX,
        int LTR_N_sub_hxrs, int HTR_N_sub_hxrs,
        double eta_mc, int mc_comp_model_code,
        double eta_rc,
        double eta_t, double N_turbine,
        double frac_fan_power, double eta_fan, double deltaP_cooler_frac,
        int N_nodes_pass,
        double T_amb_des, double elevation) :
        C_sco2_cycle_core(turbo_gen_motor_config,
            eta_generator,
            T_mc_in,
            W_dot_net,
            T_t_in, P_high_limit,
            DP_LTR, DP_HTR,
            DP_PC_main, DP_PHX,
            LTR_N_sub_hxrs, HTR_N_sub_hxrs,
            eta_mc, mc_comp_model_code,
            eta_rc,
            eta_t, N_turbine,
            frac_fan_power, eta_fan, deltaP_cooler_frac,
            N_nodes_pass,
            T_amb_des, elevation)
    {
        m_temp_last.resize(END_SCO2_HTRBP_STATES);
        std::fill(m_temp_last.begin(), m_temp_last.end(), std::numeric_limits<double>::quiet_NaN());
        m_pres_last = m_enth_last = m_entr_last = m_dens_last = m_temp_last;

        m_eta_thermal_calc_last = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = std::numeric_limits<double>::quiet_NaN();
        m_Q_dot_PHX = m_Q_dot_bypass = m_eta_bypass = std::numeric_limits<double>::quiet_NaN();
        m_W_dot_mc = m_W_dot_rc = m_W_dot_t = m_W_dot_mc_bypass = std::numeric_limits<double>::quiet_NaN();
        m_objective_metric_last = std::numeric_limits<double>::quiet_NaN();

        m_W_dot_net_last = std::numeric_limits<double>::quiet_NaN();

        m_objective_metric_opt = std::numeric_limits<double>::quiet_NaN();
        m_objective_metric_auto_opt = std::numeric_limits<double>::quiet_NaN();

        
    }

    CO2_state mc_co2_props;

    ~C_HTRBypass_Cycle() {};

    void design(S_design_parameters& des_par_in, int& error_code);

    void opt_design(S_opt_design_parameters& opt_des_par_in, int& error_code);

    void reset_ms_od_turbo_bal_csp_solved();

    int auto_opt_design(S_auto_opt_design_parameters& auto_opt_des_par_in);






    // Unused

    int auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters& auto_opt_des_hit_eta_in, std::string& error_msg);

    int off_design_fix_shaft_speeds(S_od_par& od_phi_par_in, double od_tol /*-*/);

    virtual int solve_OD_all_coolers_fan_power(double T_amb /*K*/, double od_tol /*-*/, double& W_dot_fan /*MWe*/);

    virtual int solve_OD_mc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/, double& W_dot_mc_cooler_fan /*MWe*/, double& P_co2_out /*kPa*/);

    virtual int solve_OD_pc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/, double& W_dot_pc_cooler_fan /*MWe*/, double& P_co2_out /*kPa*/);

    double get_od_temp(int n_state_point);

    double get_od_pres(int n_state_point);

    virtual void check_od_solution(double& diff_m_dot, double& diff_E_cycle,
        double& diff_Q_LTR, double& diff_Q_HTR);

    void set_od_temp(int n_state_point, double temp_K);

    void set_od_pres(int n_state_point, double pres_kPa);

    void off_design_recompressor(double T_in, double P_in, double m_dot, double P_out, double tol /*-*/, int& error_code, double& T_out);

    void estimate_od_turbo_operation(double T_mc_in /*K*/, double P_mc_in /*kPa*/, double f_recomp /*-*/, double T_t_in /*K*/, double phi_mc /*-*/,
        int& mc_error_code, double& mc_w_tip_ratio /*-*/, double& P_mc_out /*kPa*/,
        int& rc_error_code, double& rc_w_tip_ratio /*-*/, double& rc_phi /*-*/,
        bool is_update_ms_od_solved = false);

    const C_comp_multi_stage::S_od_solved* get_rc_od_solved()
    {
        return m_rc_ms.get_od_solved();
    }

    /*const S_od_turbo_bal_csp_solved* get_od_turbo_bal_csp_solved()
    {
        return &ms_od_turbo_bal_csp_solved;
    }

    double get_max_target()
    {
        return m_biggest_target;
    }*/



};

#endif
