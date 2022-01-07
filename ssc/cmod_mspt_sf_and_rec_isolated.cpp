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

static var_info _cm_vtab_mspt_sf_and_rec_isolated[] = {

    // Simulation options

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

        std::unique_ptr<C_pt_receiver> receiver;

        double H_rec = as_double("rec_height");
        double D_rec = as_double("D_rec");
        //double D_rec = H_rec / rec_aspect;

        double q_dot_rec_des = as_double("q_dot_rec_des");  //[MWt]

        int rec_night_recirc = 0;
        int rec_clearsky_model = as_integer("rec_clearsky_model");

        if (rec_clearsky_model > 4)
            throw exec_error("tcsmolten_salt", "Invalid specification for 'rec_clearsky_model'");
        if (rec_clearsky_model == -1 && as_double("rec_clearsky_fraction") >= 0.0001)
            throw exec_error("tcsmolten_salt", "'rec_clearsky_model' must be specified when 'rec_clearsky_fraction' > 0.0.");

        std::vector<double> clearsky_data;
        if (rec_clearsky_model == 0)
        {
            //size_t n_csky = 0;
            //ssc_number_t* csky = as_array("rec_clearsky_dni", &n_csky);
            //if (n_csky != n_steps_full)
            //throw exec_error("tcsmolten_salt", "Invalid clear-sky DNI data. Array must have " + util::to_string((int)n_steps_full) + " rows.");
            throw exec_error("tcsmolten_salt", "Cleary-sky data not ready");
            //
            //receiver->m_clearsky_data.resize(n_steps_full);
            //for (size_t i = 0; i < n_steps_full; i++)
            //    receiver->m_clearsky_data.at(i) = (double)csky[i];
        }

        // Transient model
        if (is_rec_model_trans || is_rec_startup_trans) {

            std::unique_ptr<C_mspt_receiver> trans_receiver = std::unique_ptr<C_mspt_receiver>(new C_mspt_receiver(
                as_double("h_tower"), as_double("epsilon"),
                as_double("T_htf_hot_des"), as_double("T_htf_cold_des"),
                as_double("f_rec_min"), q_dot_rec_des,
                as_double("rec_su_delay"), as_double("rec_qf_delay"),
                as_double("csp.pt.rec.max_oper_frac"), as_double("eta_pump"),
                rec_night_recirc, rec_clearsky_model,
                clearsky_data
                ));    // transient receiver

        }
        else { // Steady state model

            std::unique_ptr<C_mspt_receiver_222> ss_receiver = std::unique_ptr<C_mspt_receiver_222>(new C_mspt_receiver_222(
                as_double("h_tower"), as_double("epsilon"),
                as_double("T_htf_hot_des"), as_double("T_htf_cold_des"),
                as_double("f_rec_min"), q_dot_rec_des,
                as_double("rec_su_delay"), as_double("rec_qf_delay"),
                as_double("csp.pt.rec.max_oper_frac"), as_double("eta_pump"),
                rec_night_recirc, rec_clearsky_model,
                clearsky_data
                ));   // steady-state receiver

            ss_receiver->m_n_panels = as_integer("N_panels");                   //[-]
            ss_receiver->m_d_rec = D_rec;                                       //[m]
            ss_receiver->m_h_rec = H_rec;                                       //[m]
            ss_receiver->m_od_tube = as_double("d_tube_out");                   //[mm]
            ss_receiver->m_th_tube = as_double("th_tube");                      //[mm]
            ss_receiver->m_mat_tube = as_integer("mat_tube");                   //[-]
            ss_receiver->m_field_fl = as_integer("rec_htf");                    //[-]
            ss_receiver->m_field_fl_props = as_matrix("field_fl_props");        //[-]
            ss_receiver->m_flow_type = as_integer("Flow_type");                 //[-]
            ss_receiver->m_crossover_shift = as_integer("crossover_shift");     //[-]
            ss_receiver->m_hl_ffact = as_double("hl_ffact");                    //[-]
            ss_receiver->m_piping_loss_coefficient = as_double("piping_loss_coefficient");  //[W/m2-K]
            ss_receiver->m_pipe_length_add = as_double("piping_length_const");      //[m]
            ss_receiver->m_pipe_length_mult = as_double("piping_length_mult");      //[-]
            ss_receiver->m_T_salt_hot_target = as_double("T_htf_hot_des");      //[C]
            ss_receiver->m_csky_frac = as_double("rec_clearsky_fraction");

            receiver = std::move(ss_receiver);
        }

        receiver->init();

        // Receiver/tower design options
        // 1) import through cmod
        // 2) generate through solarpilot?

        // Initialization options
        // 1) Default
        // 2) Import through cmod
        // 3) Steady state (implies transient simulation?)

        // Do we want an input that allows absolute values for flux maps instead of normalized values?
        // Passing through A_sf to MSPT model to dimensionalize the flux map, which is annoying



        return;
    }
};

DEFINE_MODULE_ENTRY(mspt_sf_and_rec_isolated, "MSPT solar field and tower/receiver model", 1)
