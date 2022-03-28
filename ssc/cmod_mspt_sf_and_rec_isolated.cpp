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

#include "core.h"

#include "csp_solver_mspt_collector_receiver.h"
#include "csp_solver_mspt_receiver.h"
#include "csp_solver_mspt_receiver_222.h"

#include <algorithm>

static var_info _cm_vtab_mspt_sf_and_rec_isolated[] = {

    // Simulation options
    { SSC_INPUT,  SSC_NUMBER, "sim_type",                           "1 (default): timeseries, 2: design only",                                                               "",             "",              "Simulation",                               "?=1",                                "",              "SIMULATION_PARAMETER"},

    // Receiver design parameters
    { SSC_INPUT,  SSC_NUMBER, "q_dot_rec_des",                      "Receiver thermal power to HTF at design",                                                               "MWt",          "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_htf_cold_des",                     "Cold HTF inlet temperature at design conditions",                                                       "C",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_htf_hot_des",                      "Hot HTF outlet temperature at design conditions",                                                       "C",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INOUT,  SSC_NUMBER, "h_tower",                            "Tower height",                                                                                          "m",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_height",                         "Receiver height",                                                                                       "m",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "D_rec",                              "The overall outer diameter of the receiver",                                                            "m",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "N_panels",                           "Number of individual panels on the receiver",                                                           "",             "",              "Tower and Receiver",                       "*",                                  "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "d_tube_out",                         "The outer diameter of an individual receiver tube",                                                     "mm",           "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "th_tube",                            "The wall thickness of a single receiver tube",                                                          "mm",           "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "mat_tube",                           "Receiver tube material, 2=Stainless AISI316",                                                           "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_htf",                            "Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables",     "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "field_fl_props",                     "User defined field fluid property data",                                                                "-",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "Flow_type",                          "Receiver flow pattern: see figure on SAM Receiver page",                                                "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "crossover_shift",                    "Number of panels shift in receiver crossover position",                                                 "",             "",              "Tower and Receiver",                       "?=0",                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "epsilon",                            "The emissivity of the receiver surface coating",                                                        "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hl_ffact",                           "The heat loss factor (thermal loss fudge factor)",                                                      "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "f_rec_min",                          "Minimum receiver mass flow rate turn down fraction",                                                    "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_su_delay",                       "Fixed startup delay time for the receiver",                                                             "hr",           "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_qf_delay",                       "Energy-based receiver startup delay (fraction of rated thermal power)",                                 "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "csp.pt.rec.max_oper_frac",           "Maximum receiver mass flow rate fraction",                                                              "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "eta_pump",                           "Receiver HTF pump efficiency",                                                                          "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "piping_length_mult",                 "Piping length multiplier",                                                                              "",             "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "piping_length_const",                "Piping constant length",                                                                                "m",            "",              "Tower and Receiver",                       "*",                                  "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "piping_loss_coefficient",            "Thermal loss per meter of piping",                                                                      "Wt/m2-K",      "",              "Tower and Receiver",                       "",                                   "",              ""},

    { SSC_INPUT,  SSC_NUMBER, "rec_clearsky_model",				    "Clearsky model: None = -1, User-defined data = 0, Meinel = 1; Hottel = 2; Allen = 3; Moon = 4",         "",             "",              "Tower and Receiver",                       "?=-1",                               "",              ""},
    { SSC_INPUT,  SSC_ARRAY,  "rec_clearsky_dni",					"User-defined clear-sky DNI",                                                                            "W/m2",         "",              "Tower and Receiver",                       "rec_clearsky_model=0",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_clearsky_fraction",              "Weighting fraction on clear-sky DNI for receiver flow control",                                         "",             "",              "Tower and Receiver",                       "?=0.0",                              "",              ""},

    // Transient receiver parameters
    { SSC_INPUT,  SSC_NUMBER, "is_rec_model_trans",                 "Formulate receiver model as transient?",                                                                "",             "",              "Tower and Receiver",                       "?=0",                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "is_rec_startup_trans",               "Formulate receiver startup model as transient?",                                                        "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "rec_tm_mult",                        "Receiver thermal mass multiplier",                                                                      "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "riser_tm_mult",                      "Riser thermal mass multiplier",                                                                         "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "downc_tm_mult",                      "Downcomer thermal mass multiplier",                                                                     "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "u_riser",                            "Design point HTF velocity in riser",                                                                    "m/s",          "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "th_riser",                           "Riser or downcomer tube wall thickness",                                                                "mm",           "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "heat_trace_power",                   "Riser/downcomer heat trace power during startup",                                                       "kW/m",         "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "preheat_flux",                       "Tube absorbed solar flux during preheat",                                                               "kW/m2",        "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "min_preheat_time",                   "Minimum time required in preheat startup stage",                                                        "hr",			 "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "min_fill_time",                      "Startup time delay for filling the receiver/piping",                                                    "hr",			 "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_ramp_time",                  "Time required to reach full flux during receiver startup",                                              "hr",           "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_target_Tdiff",               "Target HTF T at end of startup - steady state hot HTF temperature",                                     "C",            "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "is_rec_startup_from_T_soln",         "Begin receiver startup from solved temperature profiles?",                                              "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "is_rec_enforce_min_startup",         "Always enforce minimum startup time",                                                                   "",             "",              "Tower and Receiver",                       "is_rec_model_trans=1",               "",              ""},

    // Receiver design
    { SSC_OUTPUT, SSC_NUMBER, "m_dot_rec_des",                      "Receiver design mass flow rate",                                                                        "kg/s",         "",              "Tower and Receiver",                       "*",                                  "",              ""},

    // Timeseries outputs
    { SSC_OUTPUT, SSC_ARRAY,  "m_dot_rec_od",                       "Receiver mass flow rate",                                                                               "kg/s",         "",              "Tower and Receiver",                       "sim_type=1",                         "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,  "eta_rec_od",                         "Receiver thermal efficiency",                                                                           "kg/s",         "",              "Tower and Receiver",                       "sim_type=1",                         "",              ""},


    var_info_invalid };

class cm_mspt_sf_and_rec_isolated : public compute_module
{
public:

    cm_mspt_sf_and_rec_isolated()
    {
        add_var_info(_cm_vtab_mspt_sf_and_rec_isolated);
    }

    void exec() override
    {
        bool is_rec_model_trans = as_boolean("is_rec_model_trans");
        bool is_rec_startup_trans = as_boolean("is_rec_startup_trans");

        std::shared_ptr<C_pt_receiver> cr_receiver;
        std::shared_ptr<C_mspt_receiver_222> mspt_base;

        double H_rec = as_double("rec_height");
        double D_rec = as_double("D_rec");
        //double D_rec = H_rec / rec_aspect;

        double q_dot_rec_des = as_double("q_dot_rec_des");  //[MWt]

        int rec_night_recirc = 0;

        // Transient model
        if (is_rec_model_trans || is_rec_startup_trans) {

            bool is_enforce_min_startup = as_boolean("is_rec_enforce_min_startup");

            //trans_receiver->m_is_startup_from_solved_profile = as_boolean("is_rec_startup_from_T_soln");
            if (as_boolean("is_rec_startup_trans") && as_boolean("is_rec_startup_from_T_soln"))
                throw exec_error("tcsmolten_salt", "Receiver startup from solved temperature profiles is only available when receiver transient startup model is enabled");

            //trans_receiver->m_is_enforce_min_startup = as_boolean("is_rec_enforce_min_startup");
            if (as_boolean("is_rec_startup_trans") && !as_boolean("is_rec_startup_from_T_soln") && !is_enforce_min_startup)
            {
                log("Both 'is_rec_enforce_min_startup' and 'is_rec_startup_from_T_soln' were set to 'false'. Minimum startup time will always be enforced unless 'is_rec_startup_from_T_soln' is set to 'true'", SSC_WARNING);
                is_enforce_min_startup = true;
            }

            std::shared_ptr<C_mspt_receiver> trans_receiver = std::shared_ptr<C_mspt_receiver>(new C_mspt_receiver(
                as_double("h_tower"), as_double("epsilon"),
                as_double("T_htf_hot_des"), as_double("T_htf_cold_des"),
                as_double("f_rec_min"), q_dot_rec_des,
                as_double("rec_su_delay"), as_double("rec_qf_delay"),
                as_double("csp.pt.rec.max_oper_frac"), as_double("eta_pump"),
                as_double("d_tube_out"), as_double("th_tube"),
                as_double("piping_loss_coefficient"), as_double("piping_length_const"),
                as_double("piping_length_mult"),
                as_integer("rec_htf"), as_matrix("field_fl_props"),
                as_integer("mat_tube"),
                rec_night_recirc,
                as_integer("N_panels"), D_rec, H_rec,
                as_integer("Flow_type"), as_integer("crossover_shift"), as_double("hl_ffact"),
                as_double("T_htf_hot_des"), as_double("rec_clearsky_fraction"),
                is_rec_model_trans, is_rec_startup_trans,
                as_double("rec_tm_mult"), as_double("u_riser"),
                as_double("th_riser"), as_double("riser_tm_mult"),
                as_double("downc_tm_mult"), as_double("heat_trace_power"),
                as_double("preheat_flux"), as_double("min_preheat_time"),
                as_double("min_fill_time"), as_double("startup_ramp_time"),
                as_double("T_htf_cold_des"), std::min(0.0, as_double("startup_target_Tdiff")),
                5.0,
                as_boolean("is_rec_startup_from_T_soln"), is_enforce_min_startup
                ));    // transient receiver

            cr_receiver = trans_receiver;
            mspt_base = trans_receiver;
        }
        else { // Steady state model

            std::shared_ptr<C_mspt_receiver_222> ss_receiver = std::shared_ptr<C_mspt_receiver_222>(new C_mspt_receiver_222(
                as_double("h_tower"), as_double("epsilon"),
                as_double("T_htf_hot_des"), as_double("T_htf_cold_des"),
                as_double("f_rec_min"), q_dot_rec_des,
                as_double("rec_su_delay"), as_double("rec_qf_delay"),
                as_double("csp.pt.rec.max_oper_frac"), as_double("eta_pump"),
                as_double("d_tube_out"), as_double("th_tube"),
                as_double("piping_loss_coefficient"), as_double("piping_length_const"),
                as_double("piping_length_mult"),
                as_integer("rec_htf"), as_matrix("field_fl_props"),
                as_integer("mat_tube"),
                rec_night_recirc,
                as_integer("N_panels"), D_rec, H_rec,
                as_integer("Flow_type"), as_integer("crossover_shift"), as_double("hl_ffact"),
                as_double("T_htf_hot_des"), as_double("rec_clearsky_fraction")
                ));   // steady-state receiver

            cr_receiver = ss_receiver; // std::copy(ss_receiver);
            mspt_base = ss_receiver;
        }

        cr_receiver->init();

        double m_dot_rec_des = std::numeric_limits<double>::quiet_NaN();
        double T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();
        int n_panels = -1;
        mspt_base->get_solved_design_common(m_dot_rec_des, T_htf_cold_des, n_panels);

        assign("m_dot_rec_des", m_dot_rec_des);

        // If only simulating receiver design then get out here
        int sim_type = as_integer("sim_type");
        if (sim_type == 2) {
            return;
        }
        // ******************************************************

        int n_runs = 2;

        // Allocate timeseries outputs
        ssc_number_t* p_m_dot_rec_od = allocate("m_dot_rec_od", n_runs);
        ssc_number_t* p_eta_rec_od = allocate("eta_rec_od", n_runs);

        for (int n_run = 0; n_run < n_runs; n_run++) {

            double step = 3600.0;       //[s]
            double P_amb = 101325.0;    //[Pa]
            double T_amb = 25.0 + 273.15;   //[K]
            double T_sky = T_amb - 15.0;    //[K]
            double I_bn = 950.0;        //[W/m2]
            double v_wind_10 = 3.0;     //[m/s]
            double clearsky_dni = std::numeric_limits<double>::quiet_NaN(); //[W/m2]
            double plant_defocus = 1.0; //[-]
            double T_salt_cold_in = T_htf_cold_des;     //[K]

            util::matrix_t<double> flux_map_input;
            flux_map_input.resize(1, n_panels);
            double dni_od = 550.0;
            for (int i = 0; i < n_panels; i++) {
                flux_map_input(0, i) = dni_od + 200*n_run;    //[W/m2]
            }

            C_csp_collector_receiver::E_csp_cr_modes input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::ON;

            // For now... Set receiver to "on" through some rec method that sets E_su and t_su to 0
            mspt_base->overwrite_startup_requirements_to_on();

            mspt_base->call(step, P_amb, T_amb, T_sky,
                I_bn, v_wind_10, clearsky_dni, plant_defocus,
                &flux_map_input, input_operation_mode,
                T_salt_cold_in);

            double m_dot_rec_od = mspt_base->ms_outputs.m_m_dot_salt_tot / 3600.0;  //[kg/s]
            double eta_rec_od = mspt_base->ms_outputs.m_eta_therm;                  //[-]

            p_m_dot_rec_od[n_run] = (ssc_number_t)m_dot_rec_od; //[kg/s]
            p_eta_rec_od[n_run] = (ssc_number_t)eta_rec_od;     //[kg/s]

            // Receiver/tower design options
            // 1) import through cmod
            // 2) generate through solarpilot?

            // Initialization options
            // 1) Default
            // 2) Import through cmod
            // 3) Steady state (implies transient simulation?)
        }



        return;
    }
};

DEFINE_MODULE_ENTRY(mspt_sf_and_rec_isolated, "MSPT solar field and tower/receiver model", 1)
