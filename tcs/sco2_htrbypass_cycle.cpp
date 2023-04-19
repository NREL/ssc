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

#include "fmin.h"



void C_HTRBypass_Cycle::design_core(int& error_code)
{
    design_core_standard(error_code);
}

void C_HTRBypass_Cycle::design_core_standard(int& error_code)
{
    // Temporary Hard coded parameters
    ms_des_par.m_recomp_frac = 0.3;
    m_cp_HTF = 1.482e+03;           // J/kg K
    m_T_HTF_PHX_inlet = 670 + 273;  // K
    m_T_HTF_BP_outlet = 770;  // K
    m_bp_frac = 0;
    m_dT_BP = 0;

    // Apply scaling to the turbomachinery here
    m_mc_ms.m_r_W_dot_scale = m_W_dot_net / 10.E3;	//[-]
    m_rc_ms.m_r_W_dot_scale = m_mc_ms.m_r_W_dot_scale;			//[-]
    m_t.m_r_W_dot_scale = m_mc_ms.m_r_W_dot_scale;				//[-]

    CO2_state co2_props;

    // Initialize Recuperators
    {
        // LTR
        mc_LT_recup.initialize(m_LTR_N_sub_hxrs, ms_des_par.m_LTR_od_UA_target_type);
        // HTR
        mc_HT_recup.initialize(m_HTR_N_sub_hxrs, ms_des_par.m_HTR_od_UA_target_type);
    }

    // Initialize a few variables
    {
        //double m_dot_t, , m_dot_rc, Q_dot_LT, Q_dot_HT, UA_LT_calc, UA_HT_calc;
        //m_dot_t = m_dot_mc = m_dot_rc = Q_dot_LT = Q_dot_HT = UA_LT_calc = UA_HT_calc = 0.0;

        m_temp_last[MC_IN] = m_T_mc_in;     //[K]
        m_pres_last[MC_IN] = 10000; // ms_des_par.m_P_mc_in
        m_pres_last[MC_OUT] = 25000; // ms_des_par.m_P_mc_out; // 25000
        m_temp_last[TURB_IN] = 923.149; // m_T_t_in;    //[K] 923.149
    }
    
    // Apply pressure drops to heat exchangers, fully defining the pressures at all states
    {
        if (m_DP_LTR[0] < 0.0)
            m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - m_pres_last[MC_OUT] * std::abs(m_DP_LTR[0]);		// relative pressure drop specified for LT recuperator (cold stream)
        else
            m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - m_DP_LTR[0];				// absolute pressure drop specified for LT recuperator (cold stream)

        if ((ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_LTR_UA < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_LTR_UA < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_LTR_min_dT < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_LTR_eff_target < 1.0E-12))
            m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT];			// If there is no LT recuperator, there is no pressure drop

        m_pres_last[MIXER_OUT] = m_pres_last[LTR_HP_OUT];			// Assume no pressure drop in mixing valve
        m_pres_last[RC_OUT] = m_pres_last[LTR_HP_OUT];				// Assume no pressure drop in mixing valve

        if (m_DP_HTR[0] < 0.0)
            m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - m_pres_last[MIXER_OUT] * std::abs(m_DP_HTR[0]);	// relative pressure drop specified for HT recuperator (cold stream)
        else
            m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - m_DP_HTR[0];				// absolute pressure drop specified for HT recuperator (cold stream)

        if ((ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_HTR_UA < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_HTR_UA < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_HTR_min_dT < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_HTR_eff_target < 1.0E-12))
            m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT];		// If there is no HT recuperator, there is no pressure drop

        if (m_DP_PHX[0] < 0.0)
            m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - m_pres_last[HTR_HP_OUT] * std::abs(m_DP_PHX[0]);	// relative pressure drop specified for PHX
        else
            m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - m_DP_PHX[0];									// absolute pressure drop specified for PHX

        if (m_DP_PC_main[1] < 0.0)
            m_pres_last[LTR_LP_OUT] = m_pres_last[MC_IN] / (1.0 - std::abs(m_DP_PC_main[1]));					// relative pressure drop specified for precooler: P1=P9-P9*rel_DP => P1=P9*(1-rel_DP)
        else
            m_pres_last[LTR_LP_OUT] = m_pres_last[MC_IN] + m_DP_PC_main[1];

        if (m_DP_LTR[1] < 0.0)
            m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] / (1.0 - std::abs(m_DP_LTR[1]));	// relative pressure drop specified for LT recuperator (hot stream)
        else
            m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] + m_DP_LTR[1];					// absolute pressure drop specified for LT recuperator (hot stream)

        if ((ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_LTR_UA < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_LTR_UA < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_LTR_min_dT < 1.0E-12)
            || (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_LTR_eff_target < 1.0E-12))
            m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT];			// if there is no LT recuperator, there is no pressure drop

        if (m_DP_HTR[1] < 0.0)
            m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] / (1.0 - std::abs(m_DP_HTR[1]));	// relative pressure drop specified for HT recuperator (hot stream)
        else
            m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] + m_DP_HTR[1];				// absolute pressure drop specified for HT recuperator (hot stream)

        if ((ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_HTR_UA < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_HTR_UA < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_HTR_min_dT < 1.0E-12)
            || (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_HTR_eff_target < 1.0E-12))
            m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT];		// if there is no HT recuperator, there is no pressure drop


        // Added pressures
        m_pres_last[BYPASS_OUT] = m_pres_last[HTR_HP_OUT];
        m_pres_last[MIXER2_OUT] = m_pres_last[HTR_HP_OUT];


    }

    // Set isentropic efficiency (temporary)
    double eta_mc_isen, eta_t_isen;
    {
        eta_mc_isen = m_eta_mc;
        eta_t_isen = m_eta_t;
    }

    // Determine the outlet state and specific work for the main compressor and turbine.
    
    // Main compressor
    m_w_mc = std::numeric_limits<double>::quiet_NaN();
    {
        int comp_error_code = 0;

        calculate_turbomachinery_outlet_1(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], eta_mc_isen, true,
            comp_error_code, m_enth_last[MC_IN], m_entr_last[MC_IN], m_dens_last[MC_IN], m_temp_last[MC_OUT],
            m_enth_last[MC_OUT], m_entr_last[MC_OUT], m_dens_last[MC_OUT], m_w_mc);

        if (comp_error_code != 0)
        {
            error_code = comp_error_code;
            return;
        }
    }

    // Turbine
    m_w_t = std::numeric_limits<double>::quiet_NaN();
    {
        int turbine_error_code = 0;

        calculate_turbomachinery_outlet_1(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], eta_t_isen, false,
            turbine_error_code, m_enth_last[TURB_IN], m_entr_last[TURB_IN], m_dens_last[TURB_IN], m_temp_last[TURB_OUT],
            m_enth_last[TURB_OUT], m_entr_last[TURB_OUT], m_dens_last[TURB_OUT], m_w_t);

        if (turbine_error_code != 0)
        {
            error_code = turbine_error_code;
            return;
        }
    }

    // Check that this cycle can produce power
    m_w_rc = std::numeric_limits<double>::quiet_NaN();
    {
        double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();

        if (ms_des_par.m_recomp_frac >= 1.E-12)
        {
            if (m_eta_rc < 0.0)		// need to convert polytropic efficiency to isentropic efficiency
            {
                int rc_error_code = 0;

                isen_eta_from_poly_eta(m_temp_last[MC_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], std::abs(m_eta_rc),
                    true, rc_error_code, eta_rc_isen);

                if (rc_error_code != 0)
                {
                    error_code = rc_error_code;
                    return;
                }
            }
            else
                eta_rc_isen = m_eta_rc;

            int rc_error_code = 0;
            calculate_turbomachinery_outlet_1(m_temp_last[MC_OUT], m_pres_last[LTR_LP_OUT], m_pres_last[RC_OUT], eta_rc_isen,
                true, rc_error_code, m_w_rc);

            if (rc_error_code != 0)
            {
                error_code = rc_error_code;
                return;
            }
        }
        else
            m_w_rc = 0.0;

        if (m_w_mc + m_w_rc + m_w_t <= 0.0)	// positive net power is impossible; return an error
        {
            error_code = 25;
            return;
        }
    }

    // ****************************************************
    // ****************************************************
    // Solve the recuperators
    {
        double T_HTR_LP_out_lower = m_temp_last[MC_OUT];		//[K] Coldest possible temperature
        double T_HTR_LP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature

        double error = 1000;
        double T_HTR_guess = 0.5 * (T_HTR_LP_out_lower + T_HTR_LP_out_upper);
        double T_HTR_calc;
        while (error > 0.1)
        {
            solve_HTR(T_HTR_guess, T_HTR_calc);
            double guess_val = T_HTR_guess;
            double calc_val = T_HTR_calc;

            error = std::abs(guess_val - calc_val);

            // Update guess value
            T_HTR_guess = 0.5 * (guess_val + calc_val);
        }
    }

    // State 5 can now be fully defined
    {
        m_enth_last[HTR_HP_OUT] = m_enth_last[MIXER_OUT] + m_Q_dot_HT / m_m_dot_t;						// Energy balance on cold stream of high-temp recuperator
        int prop_error_code = CO2_PH(m_pres_last[HTR_HP_OUT], m_enth_last[HTR_HP_OUT], &co2_props);
        if (prop_error_code != 0)
        {
            error_code = prop_error_code;
            return;
        }
        m_temp_last[HTR_HP_OUT] = co2_props.temp;
        m_entr_last[HTR_HP_OUT] = co2_props.entr;
        m_dens_last[HTR_HP_OUT] = co2_props.dens;
    }

    // Calculate cycle performance metrics (including total heat coming into cycle)
    {
        m_W_dot_mc = m_w_mc * m_m_dot_mc;		//[kWe]
        m_W_dot_rc = m_w_rc * m_m_dot_rc;		//[kWe]
        m_W_dot_t = m_w_t * m_m_dot_t;		//[kWe]
        m_W_dot_net_last = m_W_dot_mc + m_W_dot_rc + m_W_dot_t;

        m_Q_dot_pc = m_m_dot_mc * (m_enth_last[LTR_LP_OUT] - m_enth_last[MC_IN]);

        m_Q_dot_total = m_W_dot_net_last + m_Q_dot_pc;
    }

    // Define bypass out temperature
    if (true)
    {
        // Calculate Bypass Energy
        {
            // Set Bypass Temp based on HTR_HP_OUT
            m_temp_last[BYPASS_OUT] = m_temp_last[HTR_HP_OUT] + m_dT_BP;

            // Calculate BYPASS_OUT properties
            int prop_error_code = CO2_TP(this->m_temp_last[BYPASS_OUT], this->m_pres_last[BYPASS_OUT], &this->mc_co2_props);
            if (prop_error_code != 0)
            {
                return;
            }
            this->m_enth_last[BYPASS_OUT] = this->mc_co2_props.enth;
            this->m_entr_last[BYPASS_OUT] = this->mc_co2_props.entr;
            this->m_dens_last[BYPASS_OUT] = this->mc_co2_props.dens;

            // Calculate Heat Transfer in Bypass
            m_Q_dot_BP = m_m_dot_bp * (m_enth_last[BYPASS_OUT] - m_enth_last[MIXER_OUT]);
        }

        // Simulate Mixer 2
        {
            // If Bypass and HTR have flow
            if (m_bp_frac >= 1e-12 && m_bp_frac <= (1.0 - 1e-12))
            {
                m_enth_last[MIXER2_OUT] = (1.0 - m_bp_frac) * m_enth_last[HTR_HP_OUT] +
                    m_bp_frac * m_enth_last[BYPASS_OUT];	//[kJ/kg]

                int prop_error_code = CO2_PH(m_pres_last[MIXER2_OUT], m_enth_last[MIXER2_OUT], &mc_co2_props);
                if (prop_error_code != 0)
                {
                    return;
                }
                m_temp_last[MIXER2_OUT] = mc_co2_props.temp;		//[K]
                m_entr_last[MIXER2_OUT] = mc_co2_props.entr;		//[kJ/kg-K]
                m_dens_last[MIXER2_OUT] = mc_co2_props.dens;		//[kg/m^3]

            }
            // Flow only through HTR
            else if (m_bp_frac <= (1.0 - 1e-12))
            {
                m_temp_last[MIXER2_OUT] = m_temp_last[HTR_HP_OUT];		//[K]
                m_enth_last[MIXER2_OUT] = m_enth_last[HTR_HP_OUT];		//[kJ/kg]
                m_entr_last[MIXER2_OUT] = m_entr_last[HTR_HP_OUT];		//[kJ/kg-K]
                m_dens_last[MIXER2_OUT] = m_dens_last[HTR_HP_OUT];		//[kg/m^3]
            }
            // Flow only through Bypass
            else
            {
                m_temp_last[MIXER2_OUT] = m_temp_last[BYPASS_OUT];		//[K]
                m_enth_last[MIXER2_OUT] = m_enth_last[BYPASS_OUT];		//[kJ/kg]
                m_entr_last[MIXER2_OUT] = m_entr_last[BYPASS_OUT];		//[kJ/kg-K]
                m_dens_last[MIXER2_OUT] = m_dens_last[BYPASS_OUT];		//[kg/m^3]
            }
        }

        // Calculate PHX Heat Transfer
        {
            m_Q_dot_PHX = m_m_dot_t * (m_enth_last[TURB_IN] - m_enth_last[MIXER2_OUT]);
            int x = 0;
        }

        // Back Calculate and Check values
        {
            // Bypass Temps
            double bp_temp_in = m_temp_last[MIXER_OUT];
            double bp_temp_out = m_temp_last[BYPASS_OUT];

            double real_q_dot_total = m_W_dot_t + m_Q_dot_pc;

            double qSum = m_Q_dot_total;
            double qSum_calc = m_Q_dot_BP + m_Q_dot_PHX;

            int x = 0;

        }

    }

    // old bypass method
    if (false)
    {
        // Calculate Total Heat Coming into Cycle
        {
            m_W_dot_mc = m_w_mc * m_m_dot_mc;
            m_W_dot_rc = m_w_rc * m_m_dot_rc;
            m_W_dot_t = m_w_t * m_m_dot_t;
            m_Q_dot_total = m_W_dot_mc + m_W_dot_rc + m_W_dot_t + m_Q_dot_pc;
        }

        // Calculate HTF Mass Flow Rate
        {
            m_m_dot_HTF = m_Q_dot_total / ((m_T_HTF_PHX_inlet - m_T_HTF_BP_outlet) * m_cp_HTF);
        }


        // Solve Bypass Outlet Temperature
        {
            double T_BP_out_lower = m_temp_last[MIXER_OUT];		//[K] Coldest possible temperature
            double T_BP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature

            double error = 1000;
            double T_BP_guess = 0.5 * (T_BP_out_lower + T_BP_out_upper);
            double T_BP_calc;
            while (error > 0.1)
            {
                solve_bypass(T_BP_guess, T_BP_calc);
                double guess_val = T_BP_guess;
                double calc_val = T_BP_calc;

                error = std::abs(guess_val - calc_val);

                // Update guess value
                T_BP_guess = 0.5 * (guess_val + calc_val);
            }
        }
    }

    // Iterate over energy
    if (false)
    {
        // Calculate Total Heat Coming into Cycle
        {
            m_W_dot_mc = m_w_mc * m_m_dot_mc;
            m_W_dot_rc = m_w_rc * m_m_dot_rc;
            m_W_dot_t = m_w_t * m_m_dot_t;
            double m_Q_dot_pc = m_m_dot_mc * (m_enth_last[LTR_LP_OUT] - m_enth_last[MC_IN]);
            m_Q_dot_total = m_W_dot_mc + m_W_dot_rc + m_W_dot_t + m_Q_dot_pc;
        }

        // Solve Bypass Outlet Temperature
        {
            double T_BP_out_lower = m_temp_last[MIXER_OUT];		//[K] Coldest possible temperature
            double T_BP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature

            double error = 1000;
            double T_BP_guess = 0.5 * (T_BP_out_lower + T_BP_out_upper);
            double T_BP_calc;
            while (error > 0.1)
            {
                solve_bypass_energy(T_BP_guess, T_BP_calc);
                double guess_val = T_BP_guess;
                double calc_val = T_BP_calc;

                error = std::abs(guess_val - calc_val);

                // Update guess value
                T_BP_guess = 0.5 * (guess_val + calc_val);
            }

            int x = 0;
        }


    }

}
    

int C_HTRBypass_Cycle::solve_HTR(double T_HTR_LP_OUT_guess, double& T_HTR_LP_out_calc)
{
    m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = m_Q_dot_HT = std::numeric_limits<double>::quiet_NaN();
    double* diff_T_HTR_LP_out;

    // Set temperature guess
    m_temp_last[HTR_LP_OUT] = T_HTR_LP_OUT_guess;		//[K]	

    // Solve HTR_LP_OUT properties
    {
        int prop_error_code = CO2_TP(this->m_temp_last[HTR_LP_OUT], this->m_pres_last[HTR_LP_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            *diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
            return prop_error_code;
        }
        this->m_enth_last[HTR_LP_OUT] = this->mc_co2_props.enth;
        this->m_entr_last[HTR_LP_OUT] = this->mc_co2_props.entr;
        this->m_dens_last[HTR_LP_OUT] = this->mc_co2_props.dens;
    }
        

    // *********************************************************************************
    // *********************************************************************************
    // Solve for the LTR solution
    {
        double T_LTR_LP_out_lower = this->m_temp_last[MC_OUT];		//[K] Coldest possible outlet temperature
        double T_LTR_LP_out_upper = this->m_temp_last[HTR_LP_OUT];	//[K] Hottest possible outlet temperature

        double error = 1000;
        double T_LTR_guess = 0.5 * (T_LTR_LP_out_lower + T_LTR_LP_out_upper);
        double T_LTR_calc;
        while (error > 0.1)
        {
            solve_LTR(T_LTR_guess, T_LTR_calc);
            double guess_val = T_LTR_guess;
            double calc_val = T_LTR_calc;

            error = std::abs(guess_val - calc_val);

            // Update guess value
            T_LTR_guess = 0.5 * (guess_val + calc_val);

        }
    }
        
    // Know LTR performance so we can calculate the HP outlet
        // Energy balance on LTR HP stream
    {
        this->m_enth_last[LTR_HP_OUT] = this->m_enth_last[MC_OUT] + m_Q_dot_LT / m_m_dot_mc;		//[kJ/kg]
        int prop_error_code = CO2_PH(this->m_pres_last[LTR_HP_OUT], this->m_enth_last[LTR_HP_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            *diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
            return prop_error_code;
        }
        this->m_temp_last[LTR_HP_OUT] = this->mc_co2_props.temp;	//[K]
        this->m_entr_last[LTR_HP_OUT] = this->mc_co2_props.entr;	//[kJ/kg-K]
        this->m_dens_last[LTR_HP_OUT] = this->mc_co2_props.dens;	//[kg/m^3]	
    }
    
    // Simulate the Mixer
    if (this->ms_des_par.m_recomp_frac >= 1.E-12)
    {
        this->m_enth_last[MIXER_OUT] = (1.0 - this->ms_des_par.m_recomp_frac) * this->m_enth_last[LTR_HP_OUT] + this->ms_des_par.m_recomp_frac * this->m_enth_last[RC_OUT];	//[kJ/kg]
        int prop_error_code = CO2_PH(this->m_pres_last[MIXER_OUT], this->m_enth_last[MIXER_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            *diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
            return prop_error_code;
        }
        this->m_temp_last[MIXER_OUT] = this->mc_co2_props.temp;		//[K]
        this->m_entr_last[MIXER_OUT] = this->mc_co2_props.entr;		//[kJ/kg-K]
        this->m_dens_last[MIXER_OUT] = this->mc_co2_props.dens;		//[kg/m^3]
    }
    else
    {	// No recompressor, so no mixing required, and HTR HP inlet = LTR HP outlet
        this->m_temp_last[MIXER_OUT] = this->m_temp_last[LTR_HP_OUT];		//[K]
        this->m_enth_last[MIXER_OUT] = this->m_enth_last[LTR_HP_OUT];		//[kJ/kg]
        this->m_entr_last[MIXER_OUT] = this->m_entr_last[LTR_HP_OUT];		//[kJ/kg-K]
        this->m_dens_last[MIXER_OUT] = this->m_dens_last[LTR_HP_OUT];		//[kg/m^3]
    }

    // Solve Mass Flow rates for HTR_HP_OUT and Bypass
    {
        m_m_dot_bp = m_bp_frac * m_m_dot_t;
        m_m_dot_htr_hp = m_m_dot_t - m_m_dot_bp;
    }


    // Find the design solution of the HTR
    {
        T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();
        this->mc_HT_recup.design_for_target__calc_outlet(this->ms_opt_des_par.m_HTR_target_code,
            this->ms_opt_des_par.m_HTR_UA, this->ms_opt_des_par.m_HTR_min_dT, this->ms_opt_des_par.m_HTR_eff_target,
            this->ms_opt_des_par.m_HTR_eff_max,
            this->m_temp_last[MIXER_OUT], this->m_pres_last[MIXER_OUT], m_m_dot_htr_hp, this->m_pres_last[HTR_HP_OUT],
            this->m_temp_last[TURB_OUT], this->m_pres_last[TURB_OUT], m_m_dot_t, this->m_pres_last[HTR_LP_OUT],
            this->ms_opt_des_par.m_des_tol,
            m_Q_dot_HT, this->m_temp_last[HTR_HP_OUT], T_HTR_LP_out_calc);
    }

}

int C_HTRBypass_Cycle::solve_LTR(double T_LTR_LP_OUT_guess, double& T_LTR_LP_out_calc)
{
    m_w_rc = m_m_dot_t = m_m_dot_rc = m_m_dot_mc = m_Q_dot_LT = m_Q_dot_HT = std::numeric_limits<double>::quiet_NaN();
    double* diff_T_LTR_LP_out;

    // Set LTR_LP_OUT guess
    this->m_temp_last[LTR_LP_OUT] = T_LTR_LP_OUT_guess;

    // First, solve the recompressor model as necessary
    if (this->ms_des_par.m_recomp_frac >= 1.E-12)
    {
        double eta_rc_isen = std::numeric_limits<double>::quiet_NaN();

        if (this->m_eta_rc < 0.0)		// recalculate isen. efficiency of recompressor because inlet temp changes
        {
            int rc_error_code = 0;
            isen_eta_from_poly_eta(this->m_temp_last[LTR_LP_OUT], this->m_pres_last[LTR_LP_OUT],
                this->m_pres_last[RC_OUT], std::abs(this->m_eta_rc), true,
                rc_error_code, eta_rc_isen);

            if (rc_error_code != 0)
            {
                *diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
                return rc_error_code;
            }
        }
        else
        {
            eta_rc_isen = this->m_eta_rc;
        }

        int rc_error_code = 0;

        calculate_turbomachinery_outlet_1(this->m_temp_last[LTR_LP_OUT], this->m_pres_last[LTR_LP_OUT], this->m_pres_last[RC_OUT], eta_rc_isen, true, rc_error_code,
            this->m_enth_last[LTR_LP_OUT], this->m_entr_last[LTR_LP_OUT], this->m_dens_last[LTR_LP_OUT], this->m_temp_last[RC_OUT], this->m_enth_last[RC_OUT],
            this->m_entr_last[RC_OUT], this->m_dens_last[RC_OUT], m_w_rc);

        if (rc_error_code != 0)
        {
            *diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
            return rc_error_code;
        }
    }
    else
    {
        m_w_rc = 0.0;		// no recompressor
        int prop_error_code = CO2_TP(this->m_temp_last[LTR_LP_OUT], this->m_pres_last[LTR_LP_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            *diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
            return prop_error_code;
        }
        this->m_enth_last[LTR_LP_OUT] = this->mc_co2_props.enth;
        this->m_entr_last[LTR_LP_OUT] = this->mc_co2_props.entr;
        this->m_dens_last[LTR_LP_OUT] = this->mc_co2_props.dens;
        this->m_temp_last[RC_OUT] = this->m_temp_last[LTR_LP_OUT];
        this->m_enth_last[RC_OUT] = this->m_enth_last[LTR_LP_OUT];
        this->m_entr_last[RC_OUT] = this->m_entr_last[LTR_LP_OUT];
        this->m_dens_last[RC_OUT] = this->m_dens_last[LTR_LP_OUT];
    }

    // Solve Mass Flow Rates
    {
        m_m_dot_t = this->m_W_dot_net / ((m_w_mc * (1.0 - this->ms_des_par.m_recomp_frac) +
            m_w_rc * this->ms_des_par.m_recomp_frac + m_w_t) * this->m_eta_generator);		//[kg/s]

        m_m_dot_rc = m_m_dot_t * this->ms_des_par.m_recomp_frac;		//[kg/s]
        m_m_dot_mc = m_m_dot_t - m_m_dot_rc;
    }
        
    // Solve LTR
    T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();
    {
        this->mc_LT_recup.design_for_target__calc_outlet(this->ms_opt_des_par.m_LTR_target_code,
            this->ms_opt_des_par.m_LTR_UA, this->ms_opt_des_par.m_LTR_min_dT, this->ms_opt_des_par.m_LTR_eff_target,
            this->ms_opt_des_par.m_LTR_eff_max,
            this->m_temp_last[MC_OUT], this->m_pres_last[MC_OUT], m_m_dot_mc, this->m_pres_last[LTR_HP_OUT],
            this->m_temp_last[HTR_LP_OUT], this->m_pres_last[HTR_LP_OUT], m_m_dot_t, this->m_pres_last[LTR_LP_OUT],
            this->ms_opt_des_par.m_des_tol,
            m_Q_dot_LT, this->m_temp_last[LTR_HP_OUT], T_LTR_LP_out_calc);

    }
}

int C_HTRBypass_Cycle::solve_bypass(double T_BP_OUT_guess, double& T_BP_out_calc)
{
    // Set temperature guess
    m_temp_last[BYPASS_OUT] = T_BP_OUT_guess;		//[K]	

    // Solve BYPASS_OUT properties
    {
        int prop_error_code = CO2_TP(this->m_temp_last[BYPASS_OUT], this->m_pres_last[BYPASS_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            return prop_error_code;
        }
        this->m_enth_last[BYPASS_OUT] = this->mc_co2_props.enth;
        this->m_entr_last[BYPASS_OUT] = this->mc_co2_props.entr;
        this->m_dens_last[BYPASS_OUT] = this->mc_co2_props.dens;
    }

    // Simulate Mixer 2
    {
        // If Bypass and HTR have flow
        if (m_bp_frac >= 1e-12 && m_bp_frac <= (1.0-1e-12))
        {
            m_enth_last[MIXER2_OUT] = (1.0 - m_bp_frac) * m_enth_last[HTR_HP_OUT] +
                m_bp_frac * m_enth_last[BYPASS_OUT];	//[kJ/kg]

            int prop_error_code = CO2_PH(m_pres_last[MIXER2_OUT], m_enth_last[MIXER2_OUT], &mc_co2_props);
            if (prop_error_code != 0)
            {
                return prop_error_code;
            }
            m_temp_last[MIXER2_OUT] = mc_co2_props.temp;		//[K]
            m_entr_last[MIXER2_OUT] = mc_co2_props.entr;		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = mc_co2_props.dens;		//[kg/m^3]

        }
        // Flow only through HTR
        else if (m_bp_frac <= (1.0 - 1e-12))
        {
            m_temp_last[MIXER2_OUT] = m_temp_last[HTR_HP_OUT];		//[K]
            m_enth_last[MIXER2_OUT] = m_enth_last[HTR_HP_OUT];		//[kJ/kg]
            m_entr_last[MIXER2_OUT] = m_entr_last[HTR_HP_OUT];		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = m_dens_last[HTR_HP_OUT];		//[kg/m^3]
        }
        // Flow only through Bypass
        else
        {
            m_temp_last[MIXER2_OUT] = m_temp_last[BYPASS_OUT];		//[K]
            m_enth_last[MIXER2_OUT] = m_enth_last[BYPASS_OUT];		//[kJ/kg]
            m_entr_last[MIXER2_OUT] = m_entr_last[BYPASS_OUT];		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = m_dens_last[BYPASS_OUT];		//[kg/m^3]
        }
    }

    // Calculate Heat From PHX
    m_Q_dot_PHX = m_m_dot_t * (m_enth_last[TURB_IN] - m_enth_last[MIXER2_OUT]);

    // Calculate HTF PHX Outlet Temp
    m_T_HTF_PHX_out = m_T_HTF_PHX_inlet - (m_Q_dot_PHX / (m_cp_HTF * m_m_dot_HTF));

    // Check if HTF PHX Outlet is higher than BP outlet
    if (m_T_HTF_PHX_out < m_T_HTF_BP_outlet)
    {
        // Something is wrong
    }


    // Solve Bypass HTX
    {
        // Calculate Heat Transfer in Bypass
        m_Q_dot_BP = m_m_dot_HTF * m_cp_HTF * (m_T_HTF_PHX_out - m_T_HTF_BP_outlet);

        // Calculate Enthalpy at sco2 bypass outlet
        double h_bp_out_calc = (m_Q_dot_BP / m_m_dot_bp) + m_enth_last[MIXER_OUT];

        // Calculate Temperature
        int prop_error_code = CO2_PH(m_pres_last[BYPASS_OUT], h_bp_out_calc, &mc_co2_props);
        if (prop_error_code != 0)
        {
            return prop_error_code;
        }
        T_BP_out_calc = mc_co2_props.temp;

        int x = 0;
    }


    return 0;
}

int C_HTRBypass_Cycle::solve_bypass_energy(double T_BP_OUT_guess, double& T_BP_out_calc)
{
    // Set temperature guess
    m_temp_last[BYPASS_OUT] = T_BP_OUT_guess;		//[K]	

    // Solve BYPASS_OUT properties
    {
        int prop_error_code = CO2_TP(this->m_temp_last[BYPASS_OUT], this->m_pres_last[BYPASS_OUT], &this->mc_co2_props);
        if (prop_error_code != 0)
        {
            return prop_error_code;
        }
        this->m_enth_last[BYPASS_OUT] = this->mc_co2_props.enth;
        this->m_entr_last[BYPASS_OUT] = this->mc_co2_props.entr;
        this->m_dens_last[BYPASS_OUT] = this->mc_co2_props.dens;
    }

    // Calculate Bypass Heat Exchanged
    {
        m_Q_dot_BP = m_m_dot_bp * (m_enth_last[BYPASS_OUT] - m_enth_last[MIXER_OUT]);
    }

    // Simulate Mixer 2
    {
        // If Bypass and HTR have flow
        if (m_bp_frac >= 1e-12 && m_bp_frac <= (1.0 - 1e-12))
        {
            m_enth_last[MIXER2_OUT] = (1.0 - m_bp_frac) * m_enth_last[HTR_HP_OUT] +
                m_bp_frac * m_enth_last[BYPASS_OUT];	//[kJ/kg]

            int prop_error_code = CO2_PH(m_pres_last[MIXER2_OUT], m_enth_last[MIXER2_OUT], &mc_co2_props);
            if (prop_error_code != 0)
            {
                return prop_error_code;
            }
            m_temp_last[MIXER2_OUT] = mc_co2_props.temp;		//[K]
            m_entr_last[MIXER2_OUT] = mc_co2_props.entr;		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = mc_co2_props.dens;		//[kg/m^3]

        }
        // Flow only through HTR
        else if (m_bp_frac <= (1.0 - 1e-12))
        {
            m_temp_last[MIXER2_OUT] = m_temp_last[HTR_HP_OUT];		//[K]
            m_enth_last[MIXER2_OUT] = m_enth_last[HTR_HP_OUT];		//[kJ/kg]
            m_entr_last[MIXER2_OUT] = m_entr_last[HTR_HP_OUT];		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = m_dens_last[HTR_HP_OUT];		//[kg/m^3]
        }
        // Flow only through Bypass
        else
        {
            m_temp_last[MIXER2_OUT] = m_temp_last[BYPASS_OUT];		//[K]
            m_enth_last[MIXER2_OUT] = m_enth_last[BYPASS_OUT];		//[kJ/kg]
            m_entr_last[MIXER2_OUT] = m_entr_last[BYPASS_OUT];		//[kJ/kg-K]
            m_dens_last[MIXER2_OUT] = m_dens_last[BYPASS_OUT];		//[kg/m^3]
        }
    }

    // Calculate Heat From PHX
    m_Q_dot_PHX = m_m_dot_t * (m_enth_last[TURB_IN] - m_enth_last[MIXER2_OUT]);

    // Back Calculate Bypass Q_dot using Sum
    double q_dot_bp_calc = m_Q_dot_total - m_Q_dot_PHX;

    double error = q_dot_bp_calc - m_Q_dot_BP;

    // Negative Bypass energy means temperature is too low
    /*if (q_dot_bp_calc < 0)
    {
        T_BP_out_calc = m_enth_last[TURB_IN];
        return 0;
    }*/

    // Back Calculate sco2 bypass outlet temperature
    {
        double enth_bp_out_calc = m_enth_last[MIXER_OUT] + (q_dot_bp_calc / m_m_dot_bp);

        int prop_error_code = CO2_PH(m_pres_last[BYPASS_OUT], enth_bp_out_calc, &mc_co2_props);
        if (prop_error_code != 0)
        {
            return prop_error_code;
        }
        T_BP_out_calc = mc_co2_props.temp;
    }

}





void C_HTRBypass_Cycle::opt_design_core(int& error_code)
{
}

void C_HTRBypass_Cycle::auto_opt_design_core(int& error_code)
{
    // Check that simple/recomp flag is set
    if (ms_auto_opt_des_par.m_is_recomp_ok < -1.0 || (ms_auto_opt_des_par.m_is_recomp_ok > 0 &&
        ms_auto_opt_des_par.m_is_recomp_ok != 1.0 && ms_auto_opt_des_par.m_is_recomp_ok != 2.0))
    {
        throw(C_csp_exception("C_RecompCycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
            " is either between -1 and 0 (fixed recompression fraction) or equal to 1 (recomp allowed)\n"));
    }

    // map 'auto_opt_des_par_in' to 'ms_auto_opt_des_par'
        // LTR thermal design
    ms_opt_des_par.m_LTR_target_code = ms_auto_opt_des_par.m_LTR_target_code;   //[-]
    ms_opt_des_par.m_LTR_UA = ms_auto_opt_des_par.m_LTR_UA;            //[kW/K]
    ms_opt_des_par.m_LTR_min_dT = ms_auto_opt_des_par.m_LTR_min_dT;         //[K]
    ms_opt_des_par.m_LTR_eff_target = ms_auto_opt_des_par.m_LTR_eff_target; //[-]
    ms_opt_des_par.m_LTR_eff_max = ms_auto_opt_des_par.m_LTR_eff_max;    //[-]
    ms_opt_des_par.m_LTR_od_UA_target_type = ms_auto_opt_des_par.m_LTR_od_UA_target_type;
    // HTR thermal design
    ms_opt_des_par.m_HTR_target_code = ms_auto_opt_des_par.m_HTR_target_code;   //[-]
    ms_opt_des_par.m_HTR_UA = ms_auto_opt_des_par.m_HTR_UA;             //[kW/K]
    ms_opt_des_par.m_HTR_min_dT = ms_auto_opt_des_par.m_HTR_min_dT;     //[K]
    ms_opt_des_par.m_HTR_eff_target = ms_auto_opt_des_par.m_HTR_eff_target; //[-]
    ms_opt_des_par.m_HTR_eff_max = ms_auto_opt_des_par.m_HTR_eff_max;
    ms_opt_des_par.m_HTR_od_UA_target_type = ms_auto_opt_des_par.m_HTR_od_UA_target_type;
    //
    ms_opt_des_par.m_UA_rec_total = ms_auto_opt_des_par.m_UA_rec_total;
    ms_opt_des_par.m_des_tol = ms_auto_opt_des_par.m_des_tol;
    ms_opt_des_par.m_des_opt_tol = ms_auto_opt_des_par.m_des_opt_tol;

    ms_opt_des_par.m_is_des_air_cooler = ms_auto_opt_des_par.m_is_des_air_cooler;	//[-]

    ms_opt_des_par.m_des_objective_type = ms_auto_opt_des_par.m_des_objective_type;	//[-]
    ms_opt_des_par.m_min_phx_deltaT = ms_auto_opt_des_par.m_min_phx_deltaT;			//[C]

    ms_opt_des_par.m_fixed_P_mc_out = ms_auto_opt_des_par.m_fixed_P_mc_out;		//[-]

    ms_opt_des_par.m_fixed_PR_HP_to_LP = ms_auto_opt_des_par.m_fixed_PR_HP_to_LP;			//[-]

    // Outer optimization loop
    m_objective_metric_auto_opt = 0.0;

    double best_P_high = m_P_high_limit;		//[kPa]
    double PR_mc_guess = 2.5;				//[-]
    if (!ms_opt_des_par.m_fixed_P_mc_out)
    {
        double P_low_limit = std::min(m_P_high_limit, std::max(10.E3, m_P_high_limit * 0.2));		//[kPa]

        //best_P_high = fminbr(P_low_limit, m_P_high_limit, &fmin_cb_opt_des_fixed_P_high, this, 1.0);
        best_P_high = m_P_high_limit; // TEMPORARY 

        // If this runs, it should set:
            // ms_des_par_auto_opt
            // m_objective_metric_auto_opt
        // So we can update pressure ratio guess
        double PR_mc_guess_calc = ms_des_par_auto_opt.m_P_mc_out / ms_des_par_auto_opt.m_P_mc_in;

        if (std::isfinite(PR_mc_guess_calc)) {
            PR_mc_guess = PR_mc_guess_calc;
        }
        else {
            best_P_high = m_P_high_limit;       //[kPa]
        }
    }

    if (ms_auto_opt_des_par.m_is_recomp_ok != 0)
    {
        // Complete 'ms_opt_des_par' for recompression cycle
        ms_opt_des_par.m_P_mc_out_guess = best_P_high;      //[kPa]
        ms_opt_des_par.m_fixed_P_mc_out = true;

        if (ms_opt_des_par.m_fixed_PR_HP_to_LP)
        {
            ms_opt_des_par.m_PR_HP_to_LP_guess = ms_auto_opt_des_par.m_PR_HP_to_LP_guess;	//[-]
        }
        else
        {
            ms_opt_des_par.m_PR_HP_to_LP_guess = PR_mc_guess;		//[-]
        }

        // Is recompression fraction fixed or optimized?
        if (ms_auto_opt_des_par.m_is_recomp_ok < 0.0)
        {   // fixed
            ms_opt_des_par.m_recomp_frac_guess = std::abs(ms_auto_opt_des_par.m_is_recomp_ok);
            ms_opt_des_par.m_fixed_recomp_frac = true;
        }
        else
        {   // optimized
            ms_opt_des_par.m_recomp_frac_guess = 0.3;
            ms_opt_des_par.m_fixed_recomp_frac = false;
        }

        ms_opt_des_par.m_LT_frac_guess = 0.5;
        ms_opt_des_par.m_fixed_LT_frac = false;

        if (ms_opt_des_par.m_LTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_opt_des_par.m_HTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA)
        {
            ms_opt_des_par.m_fixed_LT_frac = true;
        }

        int rc_error_code = 0;

        opt_design_core(rc_error_code);

        if (rc_error_code == 0 && m_objective_metric_opt > m_objective_metric_auto_opt)
        {
            ms_des_par_auto_opt = ms_des_par_optimal;
            m_objective_metric_auto_opt = m_objective_metric_opt;
        }
    }

    // Is recompression fraction fixed or optimized?
    // If fixed, then we don't need to try simple cycle
    if (ms_auto_opt_des_par.m_is_recomp_ok == 1.0 || ms_auto_opt_des_par.m_is_recomp_ok == 0.0)
    {

        // Complete 'ms_opt_des_par' for simple cycle
        ms_opt_des_par.m_P_mc_out_guess = best_P_high;      //[kPa]
        ms_opt_des_par.m_fixed_P_mc_out = true;

        if (ms_opt_des_par.m_fixed_PR_HP_to_LP)
        {
            ms_opt_des_par.m_PR_HP_to_LP_guess = ms_auto_opt_des_par.m_PR_HP_to_LP_guess;	//[-]
        }
        else
        {
            ms_opt_des_par.m_PR_HP_to_LP_guess = PR_mc_guess;		//[-]
        }

        ms_opt_des_par.m_recomp_frac_guess = 0.0;
        ms_opt_des_par.m_fixed_recomp_frac = true;
        ms_opt_des_par.m_LT_frac_guess = 1.0;
        ms_opt_des_par.m_fixed_LT_frac = true;

        int s_error_code = 0;

        opt_design_core(s_error_code);

        if (s_error_code == 0 && m_objective_metric_opt > m_objective_metric_auto_opt)
        {
            ms_des_par_auto_opt = ms_des_par_optimal;
            m_objective_metric_auto_opt = m_objective_metric_opt;
        }
    }

    ms_des_par = ms_des_par_auto_opt;

    int optimal_design_error_code = 0;
    design_core(optimal_design_error_code);
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
    ms_auto_opt_des_par = auto_opt_des_par_in;

    int auto_opt_des_error_code = 0;

    auto_opt_design_core(auto_opt_des_error_code);

    return auto_opt_des_error_code;
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
