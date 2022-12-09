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


#include "core.h"
#include "ud_power_cycle.h"

static var_info _cm_vtab_ui_udpc_checks[] = {

    /*   VARTYPE   DATATYPE         NAME               LABEL                                            UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
    { SSC_INPUT,   SSC_MATRIX, "ud_ind_od",        "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb",                                         "",             "",                                  "User Defined Power Cycle",                 "?=[[0]]",                                                      "",              ""},
    { SSC_INPUT,   SSC_NUMBER, "T_htf_des_in",     "Input HTF design temperature",      "C", "", "", "*", "", "" },

    { SSC_INPUT,   SSC_NUMBER, "is_calc_m_dot_vs_T_amb", "0 (defalt) no; 1: return array of max m_dot vs T_amb",   "",    "",                "",              "?=0",                       "", "" },
    { SSC_INPUT,   SSC_NUMBER, "W_dot_net_des",          "Design cycle power output (no cooling parasitics)",      "MWe", "",                "System Design", "is_calc_m_dot_vs_T_amb=1",  "", "" },
    { SSC_INPUT,   SSC_NUMBER, "cooler_tot_W_dot_fan",   "Total cooler fan power",                                 "MWe", "Cooler Totals",   "",              "is_calc_m_dot_vs_T_amb=1",  "", "" },


    { SSC_OUTPUT,  SSC_NUMBER, "n_T_htf_pars",     "Number of HTF parametrics",   "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_low",        "HTF low temperature",         "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_des",        "HTF design temperature",      "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_high",       "HTF high temperature",        "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "T_htf_pars",       "HTF temperature parametric values", "C", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "n_T_amb_pars",     "Number of ambient temperature parametrics", "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_low",        "Low ambient temperature",         "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_des",        "Design ambient temperature",      "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_high",       "High ambient temperature",        "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "T_amb_pars",       "Ambient temperature parametric values", "C", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "n_m_dot_pars",     "Number of HTF mass flow parametrics",   "", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_low",        "Low normalized HTF mass flow rate",     "", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_des",        "Design normalized HTF mass flow rate",  "", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_high",       "High normalized HTF mass flow rate",    "", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "m_dot_pars",       "Normalized mass flow parametric values","", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "W_dot_gross_ND_des",   "ND cycle power output at design values of independent parameters",   "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "Q_dot_HTF_ND_des",     "ND cycle heat input at design values of independent parameters",     "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "W_dot_cooling_ND_des", "ND cycle cooling power at design values of independent parameters",  "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_water_ND_des",   "ND cycle water use at design values of independent parameters",      "C", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_ARRAY,  "T_amb_sweep",          "Ambient temperature sweep for max mass flow calcs", "", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "m_dot_htf_ND_max_vs_T_amb_rule0", "Calculated ND max htf mass flow rate vs ambient temp", "", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "m_dot_htf_ND_max_vs_T_amb_rule1", "Calculated ND max htf mass flow rate vs ambient temp", "", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_LT",             "Low temp ambient temp of calculated ND outputs",    "C", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "q_dot_ND_vs_m_dot__T_amb_LT", "Calculated ND heat in vs mass flow at LT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "W_dot_ND_vs_m_dot__T_amb_LT", "Calculated ND power in vs mass flow at LT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "eta_ND_vs_m_dot__T_amb_LT", "Calculated ND efficiency in vs mass flow at LT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_htf_ND_max_at_T_amb_LT_rule0","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "q_dot_htf_ND_max_at_T_amb_LT_rule0","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "W_dot_htf_ND_max_at_T_amb_LT_rule0","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "eta_ND_max_at_T_amb_LT_rule0","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_htf_ND_max_at_T_amb_LT_rule1","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_htf_ND_max_at_T_amb_LT_rule1","Calculated max ND HTF mass flow at LT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_HT",             "High temp ambient temp of calculated ND outputs",   "C", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "q_dot_ND_vs_m_dot__T_amb_HT", "Calculated ND heat in vs mass flow at HT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "W_dot_ND_vs_m_dot__T_amb_HT", "Calculated ND power in vs mass flow at HT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_ARRAY,  "eta_ND_vs_m_dot__T_amb_HT", "Calculated ND efficiency in vs mass flow at HT ambient temp", "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_htf_ND_max_at_T_amb_HT_rule0","Calculated max ND HTF mass flow at HT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "q_dot_htf_ND_max_at_T_amb_HT_rule0","Calculated max ND HTF mass flow at HT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "W_dot_htf_ND_max_at_T_amb_HT_rule0","Calculated max ND HTF mass flow at HT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "eta_ND_max_at_T_amb_HT_rule0","Calculated max ND HTF mass flow at HT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_htf_ND_max_at_T_amb_HT_rule1","Calculated max ND HTF mass flow at HT ambient temp",    "", "", "is_calc_m_dot_vs_T_amb=1", "", "" },

    var_info_invalid };

class cm_ui_udpc_checks : public compute_module
{
public:

    cm_ui_udpc_checks()
    {
        add_var_info(_cm_vtab_ui_udpc_checks);
    }

    void exec() override
    {
        C_ud_power_cycle c_udpc;

        int n_T_htf_pars, n_T_amb_pars, n_m_dot_pars;
        n_T_htf_pars = n_T_amb_pars = n_m_dot_pars = -1;
        double m_dot_low, m_dot_des, m_dot_high, T_htf_low, T_htf_des, T_htf_high, T_amb_low, T_amb_des, T_amb_high;
        m_dot_low = m_dot_des = m_dot_high = T_htf_low = T_htf_des = T_htf_high = T_amb_low = T_amb_des = T_amb_high = std::numeric_limits<double>::quiet_NaN();

        util::matrix_t<double> cmbd_ind = as_matrix("ud_ind_od");

        double W_dot_gross_ND_des, Q_dot_HTF_ND_des, W_dot_cooling_ND_des, m_dot_water_ND_des;
        W_dot_gross_ND_des = Q_dot_HTF_ND_des = W_dot_cooling_ND_des = m_dot_water_ND_des = std::numeric_limits<double>::quiet_NaN();

        std::vector<double> v_T_htf_unique = std::vector<double>{std::numeric_limits<double>::quiet_NaN()};
        std::vector<double> v_m_dot_unique = std::vector<double>{ std::numeric_limits<double>::quiet_NaN() };
        std::vector<double> v_T_amb_unique = std::vector<double>{ std::numeric_limits<double>::quiet_NaN() };

        double T_htf_des_in = as_double("T_htf_des_in");

        try
        {
            std::vector<double> Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs;

            c_udpc.init(cmbd_ind,
                n_T_htf_pars, n_T_amb_pars, n_m_dot_pars,
                T_htf_des, T_htf_low, T_htf_high,
                T_amb_des, T_amb_low, T_amb_high,
                m_dot_des, m_dot_low, m_dot_high,
                Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs);


            W_dot_gross_ND_des = c_udpc.get_W_dot_gross_ND(T_htf_des_in, T_amb_des, 1.0);
            Q_dot_HTF_ND_des = c_udpc.get_Q_dot_HTF_ND(T_htf_des_in, T_amb_des, 1.0);
            W_dot_cooling_ND_des = c_udpc.get_W_dot_cooling_ND(T_htf_des_in, T_amb_des, 1.0);
            m_dot_water_ND_des = c_udpc.get_m_dot_water_ND(T_htf_des_in, T_amb_des, 1.0);
        }
        catch (C_csp_exception &csp_exception)
        {
            n_T_htf_pars = n_T_amb_pars = n_m_dot_pars = -1;
            m_dot_low = m_dot_des = m_dot_high = T_htf_low = T_htf_des = T_htf_high = T_amb_low = T_amb_des = T_amb_high = std::numeric_limits<double>::quiet_NaN();

            v_T_htf_unique = std::vector<double>{ std::numeric_limits<double>::quiet_NaN() };
            v_m_dot_unique = std::vector<double>{ std::numeric_limits<double>::quiet_NaN() };
            v_T_amb_unique = std::vector<double>{ std::numeric_limits<double>::quiet_NaN() };
        }

        int is_calc_m_dot_vs_T_amb = as_integer("is_calc_m_dot_vs_T_amb");
        if (is_calc_m_dot_vs_T_amb == 1) {

            // T_amb_des = 45 or T_amb_des = 40
            double LT = 30.0;
            double HT = 35.0;

            if (T_amb_des < 39.99) {
                if (T_amb_des < 34.99) {
                    LT = 35.0;
                    HT = 40.0;
                }
                else {
                    LT = 30.0;
                    HT = 40.0;
                }
            }

            assign("T_amb_LT", LT);
            assign("T_amb_HT", HT);

            double W_dot_gross_des = as_double("W_dot_net_des");
            double W_dot_parasitic_des = as_double("cooler_tot_W_dot_fan");
            double W_dot_net_des = W_dot_gross_des - W_dot_parasitic_des;

            c_udpc.get_ind_var_params(v_T_htf_unique, v_m_dot_unique, v_T_amb_unique);
            size_t n_m_dot_steps = v_m_dot_unique.size();
            ssc_number_t* p_q_dot_ND_vs_m_dot__T_amb_LT = allocate("q_dot_ND_vs_m_dot__T_amb_LT", n_m_dot_steps);
            ssc_number_t* p_W_dot_ND_vs_m_dot__T_amb_LT = allocate("W_dot_ND_vs_m_dot__T_amb_LT", n_m_dot_steps);
            ssc_number_t* p_eta_ND_vs_m_dot__T_amb_LT = allocate("eta_ND_vs_m_dot__T_amb_LT", n_m_dot_steps);
            ssc_number_t* p_q_dot_ND_vs_m_dot__T_amb_HT = allocate("q_dot_ND_vs_m_dot__T_amb_HT", n_m_dot_steps);
            ssc_number_t* p_W_dot_ND_vs_m_dot__T_amb_HT = allocate("W_dot_ND_vs_m_dot__T_amb_HT", n_m_dot_steps);
            ssc_number_t* p_eta_ND_vs_m_dot__T_amb_HT = allocate("eta_ND_vs_m_dot__T_amb_HT", n_m_dot_steps);

            int i_m = 0;
            for (std::vector<double>::iterator i_it = v_m_dot_unique.begin(); i_it < v_m_dot_unique.end(); i_it++, i_m++) {
                p_q_dot_ND_vs_m_dot__T_amb_LT[i_m] = c_udpc.get_Q_dot_HTF_ND(T_htf_des_in, LT, *i_it);
                p_W_dot_ND_vs_m_dot__T_amb_LT[i_m] = (c_udpc.get_W_dot_gross_ND(T_htf_des_in, LT, *i_it) * W_dot_gross_des - c_udpc.get_W_dot_cooling_ND(T_htf_des_in, LT, *i_it) * W_dot_parasitic_des) / W_dot_net_des;
                p_eta_ND_vs_m_dot__T_amb_LT[i_m] = p_W_dot_ND_vs_m_dot__T_amb_LT[i_m] / p_q_dot_ND_vs_m_dot__T_amb_LT[i_m];
                p_q_dot_ND_vs_m_dot__T_amb_HT[i_m] = c_udpc.get_Q_dot_HTF_ND(T_htf_des_in, HT, *i_it);
                p_W_dot_ND_vs_m_dot__T_amb_HT[i_m] = (c_udpc.get_W_dot_gross_ND(T_htf_des_in, HT, *i_it) * W_dot_gross_des - c_udpc.get_W_dot_cooling_ND(T_htf_des_in, HT, *i_it) * W_dot_parasitic_des) / W_dot_net_des;
                p_eta_ND_vs_m_dot__T_amb_HT[i_m] = p_W_dot_ND_vs_m_dot__T_amb_HT[i_m] / p_q_dot_ND_vs_m_dot__T_amb_HT[i_m];
            }

            // Rule 0: Simple heuristic
            double m_dot_htf_ND_max0_LT, W_dot_gross_ND_max0_LT;
            c_udpc.get_max_m_dot_and_W_dot_ND(0, T_htf_des_in, LT, m_dot_high, m_dot_low,
                m_dot_htf_ND_max0_LT, W_dot_gross_ND_max0_LT);
            double q_dot_htf_ND_max0_LT = c_udpc.get_Q_dot_HTF_ND(T_htf_des_in, LT, m_dot_htf_ND_max0_LT);
            double W_dot_net_ND_max0_LT = (c_udpc.get_W_dot_gross_ND(T_htf_des_in, LT, m_dot_htf_ND_max0_LT)*W_dot_gross_des - c_udpc.get_W_dot_cooling_ND(T_htf_des_in, LT, m_dot_htf_ND_max0_LT)*W_dot_parasitic_des) / W_dot_net_des;
            assign("m_dot_htf_ND_max_at_T_amb_LT_rule0", m_dot_htf_ND_max0_LT);
            assign("q_dot_htf_ND_max_at_T_amb_LT_rule0", q_dot_htf_ND_max0_LT);
            assign("W_dot_htf_ND_max_at_T_amb_LT_rule0", W_dot_net_ND_max0_LT);
            assign("eta_ND_max_at_T_amb_LT_rule0", W_dot_net_ND_max0_LT / q_dot_htf_ND_max0_LT);
            
            double m_dot_htf_ND_max0_HT, W_dot_gross_ND_max0_HT;
            c_udpc.get_max_m_dot_and_W_dot_ND(0, T_htf_des_in, HT, m_dot_high, m_dot_low,
                m_dot_htf_ND_max0_HT, W_dot_gross_ND_max0_HT);
            double q_dot_htf_ND_max0_HT = c_udpc.get_Q_dot_HTF_ND(T_htf_des_in, HT, m_dot_htf_ND_max0_HT);
            double W_dot_net_ND_max0_HT = (c_udpc.get_W_dot_gross_ND(T_htf_des_in, HT, m_dot_htf_ND_max0_HT)*W_dot_gross_des - c_udpc.get_W_dot_cooling_ND(T_htf_des_in, HT, m_dot_htf_ND_max0_HT) * W_dot_parasitic_des) / W_dot_net_des;
            assign("m_dot_htf_ND_max_at_T_amb_HT_rule0", m_dot_htf_ND_max0_HT);
            assign("q_dot_htf_ND_max_at_T_amb_HT_rule0", q_dot_htf_ND_max0_HT);
            assign("W_dot_htf_ND_max_at_T_amb_HT_rule0", W_dot_net_ND_max0_HT);
            assign("eta_ND_max_at_T_amb_HT_rule0", W_dot_net_ND_max0_HT / q_dot_htf_ND_max0_HT);

            // Rule 1: Try to find largest m_dot where q_dot_ND is close min(1, max q_dot_ND)
            double m_dot_htf_ND_max1_LT, W_dot_gross_ND_max1_LT;
            c_udpc.get_max_m_dot_and_W_dot_ND(1, T_htf_des_in, LT, m_dot_high, m_dot_low,
                m_dot_htf_ND_max1_LT, W_dot_gross_ND_max1_LT);
            assign("m_dot_htf_ND_max_at_T_amb_LT_rule1", m_dot_htf_ND_max1_LT);

            double m_dot_htf_ND_max1_HT, W_dot_gross_ND_max1_HT;
            c_udpc.get_max_m_dot_and_W_dot_ND(1, T_htf_des_in, HT, m_dot_high, m_dot_low,
                m_dot_htf_ND_max1_HT, W_dot_gross_ND_max1_HT);
            assign("m_dot_htf_ND_max_at_T_amb_HT_rule1", m_dot_htf_ND_max1_HT);


            size_t n_amb_steps = 30;
            double delta_t_amb = (T_amb_high - T_amb_low) / (double(n_amb_steps) - 1.0);

            ssc_number_t* p_m_dot_htf_ND_max0 = allocate("m_dot_htf_ND_max_vs_T_amb_rule0", n_amb_steps);
            ssc_number_t* p_m_dot_htf_ND_max1 = allocate("m_dot_htf_ND_max_vs_T_amb_rule1", n_amb_steps);
            ssc_number_t* p_T_amb_sweep = allocate("T_amb_sweep", n_amb_steps);

            for (size_t i = 0; i < n_amb_steps; i++) {
                double i_W_dot_gross_ND_max;
                p_T_amb_sweep[i] = T_amb_low + delta_t_amb * i;
                c_udpc.get_max_m_dot_and_W_dot_ND(0, T_htf_des_in, p_T_amb_sweep[i], m_dot_high, m_dot_low,
                    p_m_dot_htf_ND_max0[i], i_W_dot_gross_ND_max);
                c_udpc.get_max_m_dot_and_W_dot_ND(1, T_htf_des_in, p_T_amb_sweep[i], m_dot_high, m_dot_low,
                    p_m_dot_htf_ND_max1[i], i_W_dot_gross_ND_max);
            }
        }

        assign("n_T_htf_pars", (ssc_number_t)n_T_htf_pars);
        assign("T_htf_low", (ssc_number_t)T_htf_low);
        assign("T_htf_des", (ssc_number_t)T_htf_des);
        assign("T_htf_high", (ssc_number_t)T_htf_high);
        ssc_number_t* p_T_htf_pars = allocate("T_htf_pars", v_T_htf_unique.size());
        for (size_t i = 0; i < v_T_htf_unique.size(); i++) {
            p_T_htf_pars[i] = v_T_htf_unique[i];
        }

        assign("n_T_amb_pars", (ssc_number_t)n_T_amb_pars);
        assign("T_amb_low", (ssc_number_t)T_amb_low);
        assign("T_amb_des", (ssc_number_t)T_amb_des);
        assign("T_amb_high", (ssc_number_t)T_amb_high);
        ssc_number_t* p_T_amb_pars = allocate("T_amb_pars", v_T_amb_unique.size());
        for (size_t i = 0; i < v_T_amb_unique.size(); i++) {
            p_T_amb_pars[i] = v_T_amb_unique[i];
        }

        assign("n_m_dot_pars", (ssc_number_t)n_m_dot_pars);
        assign("m_dot_low", (ssc_number_t)m_dot_low);
        assign("m_dot_des", (ssc_number_t)m_dot_des);
        assign("m_dot_high", (ssc_number_t)m_dot_high);
        ssc_number_t* p_m_dot_pars = allocate("m_dot_pars", v_m_dot_unique.size());
        for (size_t i = 0; i < v_m_dot_unique.size(); i++) {
            p_m_dot_pars[i] = v_m_dot_unique[i];
        }

        assign("W_dot_gross_ND_des", (ssc_number_t)W_dot_gross_ND_des);
        assign("Q_dot_HTF_ND_des", (ssc_number_t)Q_dot_HTF_ND_des);
        assign("W_dot_cooling_ND_des", (ssc_number_t)W_dot_cooling_ND_des);
        assign("m_dot_water_ND_des", (ssc_number_t)m_dot_water_ND_des);

        return;
    }
};

DEFINE_MODULE_ENTRY(ui_udpc_checks, "Calculates the levels and number of paramteric runs for 3 udpc ind variables", 0)
