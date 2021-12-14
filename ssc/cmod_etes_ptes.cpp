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

#include "common.h"
#include "csp_solver_core.h"

static var_info _cm_vtab_etes_ptes[] = {

    // Resource Data
    { SSC_INPUT,  SSC_STRING, "solar_resource_file",           "Local weather file path",                                        "",             "",                                  "Solar Resource",                           "?",                                                                "LOCAL_FILE",    ""},

    // Simulation Parameters
    { SSC_INPUT,  SSC_NUMBER, "sim_type",                      "1 (default): timeseries, 2: design only",                        "",             "",                                  "System Control",                           "?=1",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_start",                    "Simulation start time",                                          "s",            "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_stop",                     "Simulation stop time",                                           "s",            "",                                  "System Control",                           "?=31536000",                                                       "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_steps_per_hour",           "Number of simulation time steps per hour",                       "",             "",                                  "System Control",                           "?=-1",                                                             "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "vacuum_arrays",                 "Allocate arrays for only the required number of steps",          "",             "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},


    var_info_invalid };

class cm_etes_ptes : public compute_module
{
public:
    cm_etes_ptes()
    {
        add_var_info(_cm_vtab_etes_ptes);
    }

    void exec() override
    {
        // First, check sim type
        int sim_type = as_integer("sim_type");
        if (sim_type != 1 && sim_type != 2) {
            std::string sim_type_msg = util::format("sim_type input was %d. It must be 1 (timeseries) or 2 (design only)", sim_type);

            throw exec_error("etes_ptes", sim_type_msg);
        }

        // *****************************************************
        // Define generation cycle power/heat flows
            // Design parameters for capacity and efficiency
        double W_dot_gen = 100.0;   //[MWe]
        double eta_gen = 0.5;       //[-]
            // Calculate heat input
        double q_dot_hot_in_gen = W_dot_gen / eta_gen;  //[MWt]
            // Calculating q_dot_cold from power out and efficiency means that
            //      we're not accounting for cycle electrical parasitics...
            // Maybe W_dot_gen and eta should just represent "mechanical" power
            //      and account for electricity consumption as derate?
        double q_dot_cold_out_gen = W_dot_gen*(1./eta_gen - 1.0);   //[MWt]

        // Define heat pump power/heaer flows
            // Design parameters
        double heater_mult = 1.0;
        double COP_heat_charge = 1.5;  //[-]
            // Calculate heat and power 
        double q_dot_hot_out_charge = q_dot_hot_in_gen*heater_mult; //[MWt]
        double W_dot_in_charge = q_dot_hot_out_charge / COP_heat_charge;    //[MWe]
        double q_dot_cold_in_charge = W_dot_in_charge*(COP_heat_charge - 1.0);  //[MWt]

        // Check RTE and cold q dots
        double RTE = eta_gen * COP_heat_charge;     //[-]
            // is, considering heater mult, the gen q_dot_cold > the charge q_dot_cold?
        double r_q_dot__out_gen__in_charge = q_dot_cold_out_gen / (heater_mult * q_dot_cold_in_charge); //[-]
        double q_dot_cold_out_reject_gen = q_dot_cold_out_gen - heater_mult*q_dot_cold_in_charge;   //[MWt]

        // *****************************************************
        // *****************************************************
        // --- Either ----
        // Define temperatures - define TES temps using CYCLE working fluid and approach temps
            // High Temp two-tank - charging
        //double T_HT_hot_charge = 565.0;     //[C]
        //double T_HT_cold_charge = 310.0;    //[C]
        //double dT_HX_HT_charge = 5.0;       //[C] assume counterflow CR = 1
        //    // Calculate Hot Temp TES temps
        //double T_HT_hot_TES = T_HT_hot_charge - dT_HX_HT_charge;   //[C]
        //double T_HT_cold_TES = T_HT_cold_charge - dT_HX_HT_charge; //[C]
        //
        //    // Cold Temp two-tank - charging
        //double T_CT_cold_charge = -50.0;    //[C]
        //double T_CT_hot_charge = 50.0;      //[C]
        //double dT_HX_CT_charge = 5.0;       //[C] assume counterflow CR = 1
        //    // Calculate Cold Temp TES temps
        //double T_CT_cold_TES = T_CT_cold_charge + dT_HX_CT_charge;  //[C]
        //double T_CT_hot_TES = T_CT_hot_charge + dT_HX_HT_charge;    //[C]
        //
        //    // Generation temperatures
        //double dT_HX_HT_gen = 5.0;  //[C] assume counterflow CR = 1
        //double T_HT_hot_gen = T_HT_hot_TES - dT_HX_HT_gen;      //[C]
        //double T_HT_cold_gen = T_HT_cold_TES - dT_HX_HT_gen;    //[C]
        //
        //double dT_HX_CT_gen = 5.0;  //[C] assume counterflow CR = 1
        //double T_CT_hot_gen = T_CT_hot_TES + dT_HX_CT_gen;      //[C]
        //double T_CT_cold_gen = T_CT_cold_TES + dT_HX_CT_gen;    //[C]
        // *****************************************************
        // *****************************************************
        // --- OR ----
        // Define TES temps only. Assume direct storage. Heat pump and cycle model off-design uses HTF temps
        double T_HT_hot_TES = 560.0;    //[C]
        double T_HT_cold_TES = 305.0;   //[C]
        double T_CT_cold_TES = -45.0;   //[C]
        double T_CT_hot_TES = 55.0;     //[C]
        // *****************************************************

        // *****************************************************
        // System Design Calcs
        double tshours = 10.0;      //[-]
        double Q_HT_tes = q_dot_hot_out_charge*tshours;     //[MWt-hr]
        double Q_CT_tes = q_dot_cold_in_charge*tshours;     //[MWt-hr]
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Weather reader
        C_csp_weatherreader weather_reader;
        weather_reader.m_weather_data_provider = make_shared<weatherfile>(as_string("solar_resource_file"));
        if (weather_reader.m_weather_data_provider->has_message()) log(weather_reader.m_weather_data_provider->message(), SSC_WARNING);

        weather_reader.m_trackmode = 0;
        weather_reader.m_tilt = 0.0;
        weather_reader.m_azimuth = 0.0;
        // Initialize to get weather file info
        weather_reader.init();
        if (weather_reader.has_error()) throw exec_error("etes_ptes", weather_reader.get_error());
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Simulation setup
        C_csp_solver::S_sim_setup sim_setup;
        sim_setup.m_sim_time_start = as_double("time_start");       //[s] time at beginning of first time step
        sim_setup.m_sim_time_end = as_double("time_stop");          //[s] time at end of last time step

        int steps_per_hour = (int)as_double("time_steps_per_hour");     //[-]

        //if the number of steps per hour is not provided (=-1), then assign it based on the weather file step
        if (steps_per_hour < 0)
        {
            double sph_d = 3600. / weather_reader.m_weather_data_provider->step_sec();
            steps_per_hour = (int)(sph_d + 1.e-5);
            if ((double)steps_per_hour != sph_d)
                throw exec_error("etes_ptes", "The time step duration must be evenly divisible within an hour.");
        }

        size_t n_steps_fixed = (size_t)steps_per_hour * 8760;   //[-]
        if (as_boolean("vacuum_arrays"))
        {
            n_steps_fixed = steps_per_hour * (size_t)((sim_setup.m_sim_time_end - sim_setup.m_sim_time_start) / 3600.);
        }
        sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;  //[s]
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Power cycle


        return;
    }
};

DEFINE_MODULE_ENTRY(etes_ptes, "Heat pump charging two two-tank TES from grid, discharge with power cycle", 1)
