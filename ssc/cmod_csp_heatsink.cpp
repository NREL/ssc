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
#include "water_properties.h"
#include "heat_exchangers.h"

static var_info _cm_vtab_csp_heatsink[] = {
    /* VARTYPE          DATATYPE         NAME                         LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS         UI_HINTS*/
    // Inputs
    { SSC_INPUT,        SSC_NUMBER,      "t_step",                    "Timestep duration",                                                                "s",            "",               "system",         "*",                       "",                      "" },
    
    var_info_invalid };

class cm_csp_heatsink : public compute_module
{
public:
    cm_csp_heatsink()
    {
        add_var_info(_cm_vtab_csp_heatsink);
    }

    void exec()
    {

        double dT_subcool = 30;         //[dC] Steam temp diff below outlet

        // Define Outlet Steam Conditions
        water_state ms_water_props;
        double Q_ext_hot = 0.75;    // Outlet Steam Quality
        double T_ext_hot = 150;     // [C] Outlet Steam Temp

        int prop_error_code = water_TQ(T_ext_hot + 273.15, Q_ext_hot, &ms_water_props);

        double P_ext_hot = ms_water_props.pres; // [kPa] Outlet Steam Pressure
        double h_ext_hot = ms_water_props.enth; // [kJ/kg] Outlet Steam Enthalpy
        double dens_ext_hot = ms_water_props.dens;  //[kg/m3] Outlet steam density
        double k_ext_hot = water_cond(dens_ext_hot, T_ext_hot + 273.15);    // [W/m-K]
        double mu_ext_hot = water_visc(dens_ext_hot, T_ext_hot + 273.15);   // [uPa-s]

        // Define Inlet Steam Conditions
        double T_ext_cold = T_ext_hot - dT_subcool; // [C] Inlet water temp
        double P_ext_cold = P_ext_hot;              // [kPa] Inlet Steam Pressure

        prop_error_code = water_TP(T_ext_cold + 273.15, P_ext_cold, &ms_water_props);

        double h_ext_cold = ms_water_props.enth;    // [kJ/kg] Inlet water enthalpy
        double Q_ext_cold = ms_water_props.qual;    // [] Inlet water quality (should be 0, n/a)
        double dens_ext_cold = ms_water_props.dens; // [kg/m3] Inlet water density
        double k_ext_cold = water_cond(dens_ext_cold, T_ext_cold + 273.15);    // [W/m-K]
        double mu_ext_cold = water_visc(dens_ext_cold, T_ext_cold + 273.15);   // [uPa-s]

        // Initialize
        C_HX_htf_to_steam m_hx;
        int hot_fl = 21;    // HTF fl id
        int N_sub_hx = 5000;
        NS_HX_counterflow_eqs::E_UA_target_type od_target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_constant_UA;
        m_hx.initialize(hot_fl, N_sub_hx, od_target_type);

        // Design
        double T_htf_hot = 250;     //[C]
        double T_htf_cold = 200;    //[C]
        double q_design = 5.19;     //[MW]
        C_HX_counterflow_CRM::S_des_solved des_solved;
        m_hx.design_w_TP_PH(T_htf_hot + 273.15, 1.0, T_htf_cold + 273.15, 1.0,
            P_ext_cold, h_ext_cold, P_ext_hot, h_ext_hot, q_design * 1e3, des_solved);

        // Off Design
        double T_htf_hot_od = 250;
        double T_htf_cold_od = 200;
        double h_htf_hot_od = m_hx.mc_hot_fl.enth(T_htf_hot_od + 273.15) * 1e-3;   //[kJ/kg]
        double h_htf_cold_od = m_hx.mc_hot_fl.enth(T_htf_cold_od + 273.15) * 1e-3;   //[kJ/kg]

        double q_dot_calc, h_ext_out_calc, h_htf_out_calc;

        m_hx.off_design_solution_fixed_dP_enth(h_ext_cold, P_ext_cold, m_hx.ms_des_calc_UA_par.m_m_dot_cold_des, P_ext_hot,
            h_htf_hot_od, 1.0, m_hx.ms_des_calc_UA_par.m_m_dot_hot_des, 1.0, 1e-12,
            q_dot_calc, h_ext_out_calc, h_htf_out_calc);

        double mdot_ext_calc;

        m_hx.off_design_target_cold_PH_out(h_ext_hot, 0.1, 2* m_hx.ms_des_calc_UA_par.m_m_dot_cold_des, P_ext_cold, h_ext_cold,
            P_ext_hot, 1.0, h_htf_hot_od, 1.0, m_hx.ms_des_calc_UA_par.m_m_dot_hot_des, 1e-3,
            q_dot_calc, h_ext_out_calc, h_htf_out_calc, mdot_ext_calc);


        int x = 0;
    }
};

DEFINE_MODULE_ENTRY(csp_heatsink, "CSP heat sink", 1)
