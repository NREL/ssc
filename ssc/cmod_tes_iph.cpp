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

// Trough CSP - physical model
#include "core.h"
//#include "tckernel.h"

// for adjustment factors
#include "common.h"

//#include "lib_weatherfile.h
//#include "csp_solver_util.h"
#include "csp_solver_core.h"
#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_pc_heat_sink.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_dispatch.h"
#include "csp_system_costs.h"
//#include "cmod_csp_common_eqns.h"

#include <ctime>
#include <algorithm>
#include <iterator>

// signed/unsigned mismatch
#pragma warning (disable : 4388)

static var_info _cm_vtab_tes_iph[] = {


    
    // TES
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "T_loop_in_des",                                                                    "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "T_loop_out",                                                                       "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",                "solar_mult",                                                                       "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",               "q_pb_design",                                                                      "MW",           "",               "TES",            "*",                       "",                      "" },


    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                                                "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                                         "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                                       "hr",           "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",                            "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                                   "W/m2-K",       "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                                  "-",            "",               "TES",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                                              "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",         "Rated heater capacity for hot tank heating",                                       "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                                             "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",        "Rated heater capacity for cold tank heating",                                      "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                                        "C",            "",               "TES",            "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "dt_cold",                   "Cold side HX approach temp",                                                       "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",            "Initial hot tank fluid tmeperature",                                               "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Initial cold tank fluid tmeperature",                                              "C",            "",               "TES",            "*",                      "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                                     "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",      "Initial fraction of avail. vol that is hot",                                       "%",            "",               "TES",            "*",                       "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "custom_sf_pipe_sizes",      "Use custom solar field pipe diams, wallthks, and lengths",                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_diams",              "Custom runner diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_wallthicks",         "Custom runner wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_lengths",            "Custom runner lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_diams",              "Custom header diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_wallthicks",         "Custom header wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_lengths",            "Custom header lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tanks_in_parallel",         "Tanks are in parallel, not in series, with solar field",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "has_hot_tank_bypass",       "Bypass valve connects field outlet to cold tank",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_inlet_min",      "Minimum hot tank htf inlet temperature",                                           "C",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",                                "kW/(kg/s)",    "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tes_des",                 "Design-point velocity to size the TES pipe diameters",                             "m/s",          "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_p_loss",         "TES pipe losses are based on custom lengths and coeffs",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "k_tes_loss_coeffs",         "Minor loss coeffs for the coll, gen, and bypass loops",                            "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_pipe_sizes",     "Use custom TES pipe diams, wallthks, and lengths",                                 "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_diams",                 "Custom TES diameters",                                                             "m",            "",               "controller",     "custom_tes_pipe_sizes=1",                      "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_wallthicks",            "Custom TES wall thicknesses",                                                      "m",            "",               "controller",     "custom_tes_pipe_sizes=1", "",                      "SIMULATION_PARAMETER" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_lengths",               "Custom TES lengths",                                                               "m",            "",               "controller",     "custom_tes_pipe_sizes=1", "",                      "SIMULATION_PARAMETER" },

    // *************************************************************************************************
    //    OUTPUTS
    // *************************************************************************************************

    // Thermal Storage

    { SSC_OUTPUT,       SSC_NUMBER,      "vol_tank",                         "Total tank volume",                                                        "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "q_tes",                            "TES design capacity",                                                      "MWt-hr",        "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_pt_tes_tank_diameter",         "Tank diameter",                                                            "m",             "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "q_dot_tes_est",                    "Estimated TES Heat Loss",                                                  "MW",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "csp_pt_tes_htf_density",           "Storage htf density",                                                      "kg/m3",         "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_avail_vol",                    "Available HTF volume",                                                     "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "is_hx",                            "System has heat exchanger no/yes (0/1)",                                   "",              "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "vol_min",                          "Minimum Fluid Volume",                                                     "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "V_tank_hot_ini",                   "Initial hot tank volume",                                                  "m3",            "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_avg_temp",                 "HTF Average Temperature at Design",                                        "C",             "",               "Thermal Storage","*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_min_temp",                 "Minimum storage htf temp",                                                 "C",             "",               "Power Cycle",    "*",                                "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "tes_htf_max_temp",                 "Maximum storage htf temp",                                                 "C",             "",               "Power Cycle",    "*",                                "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "timestep",             "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb",                "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_cr_to_cv_hot",   "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_cv_hot_to_sink", "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_cr_to_cv_cold",  "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cr_out_hot",         "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_sink_out_cold",      "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "T_sink_htf_in_hot",     "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "T_cr_in_cold",          "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "T_tank_hot",           "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "T_tank_cold",          "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "hot_tank_vol_frac",          "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },

    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",       "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",      "custom cmod inputs", "tbd", "", "Thermal Storage", "*", "", "" },
    

    var_info_invalid };
    
    
    
class cm_tes_iph : public compute_module
{
public:

    cm_tes_iph()
    {
        add_var_info( _cm_vtab_tes_iph );
        //add_var_info( vtab_adjustment_factors );
        //add_var_info(vtab_technology_outputs);
    }

    void exec( )
    {
        // Uncomment following 2 lines to write cmod inputs to LK script
        //FILE* fp = fopen("trough_iph_cmod_to_lk.lk", "w");
        //write_cmod_to_lk_script(fp, m_vartab);
        // Common Parameters
        double T_htf_cold_des = as_double("T_loop_in_des");    //[C]
        double T_htf_hot_des = as_double("T_loop_out");      //[C]
        double tshours = as_double("tshours");                  //[-]
        double q_dot_pc_des = as_double("q_pb_design");         //[MWt] HEAT SINK design thermal power
        double Q_tes = q_dot_pc_des * tshours;                  //[MWt-hr]
        int is_dispatch = 0;

        // *****************************************************
        // System Design Parameters

        // ********************************
        // ********************************
        // TES
        // ********************************
        // ********************************
        C_csp_two_tank_tes storage;
        {

            bool custom_tes_pipe_sizes = as_boolean("custom_tes_pipe_sizes");
            util::matrix_t<double> tes_lengths;
            if (is_assigned("tes_lengths")) {
                tes_lengths = as_matrix("tes_lengths");               //[m]
            }
            if (!is_assigned("tes_lengths") || tes_lengths.ncells() < 11) {
                double vals1[11] = { 0., 90., 100., 120., 0., 30., 90., 80., 80., 120., 80. };
                tes_lengths.assign(vals1, 11);
            }
            util::matrix_t<double> tes_wallthicks;
            if (!is_assigned("tes_wallthicks"))
            {
                double tes_wallthicks_val[1] = { -1 };
                tes_wallthicks.assign(tes_wallthicks_val, 1);
            }
            util::matrix_t<double> tes_diams;
            if (!is_assigned("tes_diams"))
            {
                double tes_diams_val[1] = { -1 };
                tes_diams.assign(tes_diams_val, 1);
            }

            storage = C_csp_two_tank_tes(
                as_integer("store_fluid"),
                as_matrix("store_fl_props"),
                as_integer("store_fluid"),
                as_matrix("store_fl_props"),
                q_dot_pc_des,
                as_double("solar_mult"),
                Q_tes,
                as_double("h_tank"),
                as_double("u_tank"),
                as_integer("tank_pairs"),
                as_double("hot_tank_Thtr"),
                as_double("hot_tank_max_heat"),
                as_double("cold_tank_Thtr"),
                as_double("cold_tank_max_heat"),
                as_double("dt_hot"),
                as_double("T_loop_in_des"),
                as_double("T_loop_out"),
                as_double("T_tank_hot_ini"),
                as_double("T_tank_cold_ini"),
                as_double("h_tank_min"),
                as_double("init_hot_htf_percent"),
                as_double("pb_pump_coef"),
                as_boolean("tanks_in_parallel"),
                as_double("V_tes_des"),
                as_boolean("calc_design_pipe_vals"),
                as_double("tes_pump_coef"),
                as_double("eta_pump"),
                as_boolean("has_hot_tank_bypass"),
                as_double("T_tank_hot_inlet_min"),
                as_boolean("custom_tes_p_loss"),
                as_boolean("custom_tes_pipe_sizes"),
                as_matrix("k_tes_loss_coeffs"),
                tes_diams,
                tes_wallthicks,
                tes_lengths,
                as_double("HDR_rough")
            );

        }


        // ********************************
        // Initialize TES (Design Point Calc)
        // ********************************

        C_csp_tes::S_csp_tes_init_inputs init_inputs;
        init_inputs.T_to_cr_at_des = as_double("T_loop_in_des") + 273.15;        // [K]
        init_inputs.T_from_cr_at_des = as_double("T_loop_out") + 273.15;         // [K]
        init_inputs.P_to_cr_at_des = 19.64;                                     // [bar]
        storage.init(init_inputs);


        // ********************************
        // ********************************
        // Report Design Point Calculations to UI
        // ********************************
        // ********************************
        {
            double V_tes_htf_avail_calc /*m3*/, V_tes_htf_total_calc /*m3*/,
                d_tank_calc /*m*/, q_dot_loss_tes_des_calc /*MWt*/, dens_store_htf_at_T_ave_calc /*kg/m3*/,
                Q_tes_des_calc /*MWt-hr*/;

            storage.get_design_parameters(V_tes_htf_avail_calc, V_tes_htf_total_calc,
                d_tank_calc, q_dot_loss_tes_des_calc, dens_store_htf_at_T_ave_calc, Q_tes_des_calc);

            double vol_min = V_tes_htf_total_calc * (storage.m_h_tank_min / storage.m_h_tank);
            double V_tank_hot_ini = (as_double("h_tank_min") / as_double("h_tank")) * V_tes_htf_total_calc; // m3
            double T_avg = (as_double("T_loop_in_des") + as_double("T_loop_out")) / 2.0;    // C
            double tes_htf_min_temp = storage.get_min_storage_htf_temp() - 273.15;
            double tes_htf_max_temp = storage.get_max_storage_htf_temp() - 273.15;

            assign("q_tes", Q_tes_des_calc); // MWt-hr
            assign("tes_avail_vol", V_tes_htf_avail_calc); // m3
            assign("vol_tank", V_tes_htf_total_calc);   // m3
            assign("csp_pt_tes_tank_diameter", d_tank_calc);    // m
            assign("q_dot_tes_est", q_dot_loss_tes_des_calc);   // MWt
            assign("csp_pt_tes_htf_density", dens_store_htf_at_T_ave_calc); // kg/m3
            assign("is_hx", storage.get_is_hx());
            assign("vol_min", vol_min); // m3
            assign("V_tank_hot_ini", V_tank_hot_ini);   // m3
            assign("tes_htf_avg_temp", T_avg);  // C
            assign("tes_htf_min_temp", tes_htf_min_temp);
            assign("tes_htf_max_temp", tes_htf_max_temp);
        }

        // Simulate TES
        double timestep = as_double("timestep");
        double T_amb = as_double("T_amb");
        double m_dot_cr_to_cv_hot = as_double("m_dot_cr_to_cv_hot");
        double m_dot_cv_hot_to_sink = as_double("m_dot_cv_hot_to_sink");
        double m_dot_cr_to_cv_cold = as_double("m_dot_cr_to_cv_cold");
        double T_cr_out_hot = as_double("T_cr_out_hot");
        double T_sink_out_cold = as_double("T_sink_out_cold");
        double T_sink_htf_in_hot; /*K*/ // &: reference values
        double T_cr_in_cold;  /*K*/
        //init_inputs.T_to_cr_at_des = as_double("T_loop_in_des") + 273.15;        // [K]
        C_csp_tes::S_csp_tes_outputs outputs;
        storage.solve_tes_off_design(
            timestep, T_amb,
            m_dot_cr_to_cv_hot, m_dot_cv_hot_to_sink, m_dot_cr_to_cv_cold,
            T_cr_out_hot, T_sink_out_cold,
            T_sink_htf_in_hot, T_cr_in_cold, outputs
        );
        double T_tank_hot = storage.get_hot_temp();
        double T_tank_cold = storage.get_cold_temp();
        double hot_tank_vol_frac = storage.get_hot_tank_vol_frac();

        // Assign outputs
        assign("T_sink_htf_in_hot", T_sink_htf_in_hot);  // units
        assign("T_cr_in_cold", T_cr_in_cold);  // units
        assign("T_tank_hot", T_tank_hot);  // units
        assign("T_tank_cold", T_tank_cold);  // units
        assign("hot_tank_vol_frac", hot_tank_vol_frac);  // units
        
        // If no exception, then report messages


        // Do unit post-processing here


        // Convert mass flow rates from [kg/hr] to [kg/s]


    }

    template <typename T>
    void set_vector(const std::string& name, const vector<T> vec)
    {
        int size = vec.size();
        ssc_number_t* alloc_vals = allocate(name, size);
        for (int i = 0; i < size; i++)
            alloc_vals[i] = vec[i];    // []
    }

};

DEFINE_MODULE_ENTRY(tes_iph, "Physical trough iph applications", 1)
