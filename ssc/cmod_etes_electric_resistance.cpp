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

#include "csp_solver_cr_electric_resistance.h"

static var_info _cm_vtab_etes_electric_resistance[] = {

    // System Design
    { SSC_INPUT,  SSC_NUMBER, "T_htf_cold_des",                "Cold HTF inlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_htf_hot_des",                 "Hot HTF outlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_ref",                         "Reference output electric power at design condition",            "MW",           "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "design_eff",                    "Power cycle efficiency at design",                               "none",         "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tshours",                       "Equivalent full-load thermal storage hours",                     "hr",           "",                                  "System Design",                            "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_NUMBER, "heater_mult",                   "Heater multiple relative to design cycle thermal power",         "-",            "",                                  "System Design",                            "*",                                                                "",              ""},

    // TES Design
/*new*/    { SSC_INPUT,  SSC_NUMBER, "tes_fl_code",                   "Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables", "", "",           "TES",                                      "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_MATRIX, "ud_tes_fl_props",               "User-defined TES fluid property data",                           "-",            "",                                  "TES",                                      "*",                                                                "",              ""},


    var_info_invalid };

class cm_etes_electric_resistance : public compute_module
{
public:

    cm_etes_electric_resistance()
    {
        add_var_info(_cm_vtab_etes_electric_resistance);
    }

    void exec() override
    {
        // System Design Parameters
        double T_htf_cold_des = as_double("T_htf_cold_des");    //[C]
        double T_htf_hot_des = as_double("T_htf_hot_des");      //[C]
        double W_dot_cycle_des = as_double("P_ref");            //[MWe]
        double eta_cycle = as_double("design_eff");             //[-]
        double tshours = as_double("tshours");                  //[-]
        double heater_mult = as_double("heater_mult");          //[-]

        // System Design Calcs
        double q_dot_pc_des = W_dot_cycle_des / eta_cycle;      //[MWt]
        double q_dot_heater_des = q_dot_pc_des * heater_mult;   //[MWt]

        // TES parameters
        int tes_fl_code = as_integer("tes_fl_code");
        util::matrix_t<double> ud_tes_fl_props = as_matrix("ud_tes_fl_props");

        // Construct electric resistance heater class
        C_csp_cr_electric_resistance c_electric_resistance(T_htf_cold_des, T_htf_hot_des, q_dot_heater_des,
            tes_fl_code, ud_tes_fl_props);

        // Test init()
        C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
        C_csp_collector_receiver::S_csp_cr_solved_params solved_params;
        c_electric_resistance.init(init_inputs, solved_params);
    }
};

DEFINE_MODULE_ENTRY(etes_electric_resistance, "Electric resistance heater charging TES from grid, discharge with power cycle", 1)
