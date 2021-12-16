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

#include "csp_solver_pc_ptes.h"
#include "csp_solver_cr_heat_pump.h"
#include "csp_solver_two_tank_tes.h"

static var_info _cm_vtab_etes_ptes[] = {

    // Resource Data
    { SSC_INPUT,  SSC_STRING, "solar_resource_file",           "Local weather file path",                                        "",             "",                                  "Solar Resource",                           "?",                                                                "LOCAL_FILE",    ""},

    // Simulation Parameters
    { SSC_INPUT,  SSC_NUMBER, "sim_type",                      "1 (default): timeseries, 2: design only",                        "",             "",                                  "System Control",                           "?=1",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_start",                    "Simulation start time",                                          "s",            "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_stop",                     "Simulation stop time",                                           "s",            "",                                  "System Control",                           "?=31536000",                                                       "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "time_steps_per_hour",           "Number of simulation time steps per hour",                       "",             "",                                  "System Control",                           "?=-1",                                                             "",              "SIMULATION_PARAMETER"},
    { SSC_INPUT,  SSC_NUMBER, "vacuum_arrays",                 "Allocate arrays for only the required number of steps",          "",             "",                                  "System Control",                           "?=0",                                                              "",              "SIMULATION_PARAMETER"},


    // HTFs
    { SSC_INPUT,  SSC_NUMBER, "hot_htf_code",                  "Hot HTF code - see htf_props.h for list",                        "",             "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_hot_htf_props",              "User-defined Hot HTF fluid property data",                       "-",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_htf_code",                 "Cold HTF code - see htf_props.h for list",                       "",             "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_cold_htf_props",             "User-defined Cold HTF fluid property data",                      "-",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},


    // Heat Pump
    { SSC_INPUT,  SSC_NUMBER, "f_q_dot_des_allowable_su",      "Fraction of design power allowed during startup",                "-",            "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hrs_startup_at_max_rate",       "Duration of startup at max startup power",                       "hr",           "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "f_q_dot_heater_min",            "Minimum allowable heater output as fraction of design",          "",             "",                                  "Heater",                                   "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "heat_pump_HT_HTF_pump_coef",    "High temp HX pumping power to move 1 kg/s",                      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "heat_pump_CT_HTF_pump_coef",    "Cold temp HX pumping power to move 1 kg/s",                      "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},


    // Power Cycle
        // General
    { SSC_INPUT,  SSC_NUMBER, "pb_pump_coef",                  "Pumping power to move 1kg of HTF through PB loop",               "kW/kg/s",      "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_time",                  "Time needed for power block startup",                            "hr",           "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_frac",                  "Fraction of design thermal power needed for startup",            "none",         "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_max_frac",                "Maximum turbine over design operation fraction",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_cutoff_frac",             "Minimum turbine operation fraction before shutdown",             "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "q_sby_frac",                    "Fraction of thermal power required for standby",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},


    // High Temp Two-Tank TES
    { SSC_INPUT,  SSC_NUMBER, "tes_init_hot_htf_percent",      "HOT TES Initial fraction of available volume that is hot",       "%",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank",                        "HOT TES Total height of tank (height of HTF when tank is full)", "m",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_max_heat",            "HOT TES Rated heater capacity for cold tank heating",            "MW",           "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "u_tank",                        "HOT TES Loss coefficient from the tank",                         "W/m2-K",       "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tank_pairs",                    "HOT TES Number of equivalent tank pairs",                        "",             "",                                  "Hot Thermal Storage",                      "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_Thtr",                "HOT TES Minimum allowable cold tank HTF temperature",            "C",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank_min",                    "HOT TES Minimum allowable HTF height in storage tank",           "m",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_Thtr",                 "HOT TES Minimum allowable hot tank HTF temperature",             "C",            "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_max_heat",             "HOT TES Rated heater capacity for hot tank heating",             "MW",           "",                                  "Hot Thermal Storage",                      "*",                                                                "",              ""},


    // COLD Temp Two-Tank TES
    { SSC_INPUT,  SSC_NUMBER, "CT_tes_init_hot_htf_percent",   "COLD TES Initial fraction of available volume that is hot",      "%",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_h_tank",                     "COLD TES Total height of tank (height of HTF when tank is full)","m",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_u_tank",                     "COLD TES Loss coefficient from the tank",                        "W/m2-K",       "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_tank_pairs",                 "COLD TES Number of equivalent tank pairs",                       "",             "",                                  "Cold Thermal Storage",                     "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "CT_h_tank_min",                 "COLD TES Minimum allowable HTF height in storage tank",          "m",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},

    //{ SSC_INPUT,  SSC_NUMBER, "CT_cold_tank_max_heat",         "COLD TES Rated heater capacity for cold tank heating",           "MW",           "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_cold_tank_Thtr",             "COLD TES Minimum allowable cold tank HTF temperature",           "C",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_hot_tank_Thtr",              "COLD TES Minimum allowable hot tank HTF temperature",            "C",            "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},
    //{ SSC_INPUT,  SSC_NUMBER, "CT_hot_tank_max_heat",          "COLD TES Rated heater capacity for hot tank heating",            "MW",           "",                                  "Cold Thermal Storage",                     "*",                                                                "",              ""},


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

        // Define generation mechanical, electrical, and thermal power
        // Need to break out thermodynamic cycle so that net output, heat input, heat output, and efficiency are consistent
        // Important because: 1) ideal gas cycles could have significant compressor loads and 2) important to capture heat rejection for CT storage
        double W_dot_gen_thermo = 100.0;   //[MWe]
        double W_dot_gen_used_elec = 10.0;   //[MWe]
        double W_dot_gen_net = W_dot_gen_thermo - W_dot_gen_used_elec;  //[MWe]
        double eta_therm_mech = 0.5;     //[-]
        double q_dot_hot_in_gen = W_dot_gen_thermo / eta_therm_mech;    //[MWt]
        double q_dot_cold_out_gen = W_dot_gen_thermo*(1./eta_therm_mech - 1.);     //[MWt]
        double eta_overall = W_dot_gen_net / q_dot_hot_in_gen;      //[-]

        // Define heat pump power/heat flows
            // Design parameters
        double heater_mult = 1.0;
        double COP_heat_charge_therm = 1.5;     //[-]
        double f_elec_consume_vs_W_dot_thermo = 0.05;   //[-]
            // Calculate heat and power 
        double q_dot_hot_out_charge = q_dot_hot_in_gen*heater_mult; //[MWt]
        double W_dot_in_charge_thermo = q_dot_hot_out_charge / COP_heat_charge_therm;       //[MWe] COP_heat = q_dot_hot_out_charge / W_dot_in_charge_thermo
        double q_dot_cold_in_charge = W_dot_in_charge_thermo*(COP_heat_charge_therm - 1.0); //[MWt]
        double W_dot_in_charge_elec = f_elec_consume_vs_W_dot_thermo*W_dot_in_charge_thermo;  //[MWe]
        double W_dot_charge_net = W_dot_in_charge_thermo + W_dot_in_charge_elec;        //[MWe]
        double COP_heat_charge_net = q_dot_hot_out_charge / W_dot_charge_net;           //[-]

        // Check RTE and cold q dots
        double fixed__q_dot_cold__to__q_dot_warm = q_dot_cold_in_charge / q_dot_hot_out_charge;

        double RTE_therm = eta_therm_mech * COP_heat_charge_therm;     //[-]

        double q_dot_cold_out_gen_to_CTES = fixed__q_dot_cold__to__q_dot_warm * q_dot_hot_in_gen;   //[MWt]
        double q_dot_cold_out_gen_to_surr = q_dot_cold_out_gen - q_dot_cold_out_gen_to_CTES;        //[MWt]

            // is, considering heater mult, the gen q_dot_cold > the charge q_dot_cold?
        //double r_q_dot__out_gen__in_charge = q_dot_cold_out_gen / (heater_mult * q_dot_cold_in_charge); //[-]
        //double q_dot_cold_out_reject_gen = q_dot_cold_out_gen - heater_mult*q_dot_cold_in_charge;   //[MWt]
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

        // Get HTF inputs here
        int HT_htf_code = as_integer("hot_htf_code");
        util::matrix_t<double> ud_HT_htf_props = as_matrix("ud_hot_htf_props");
        int CT_htf_code = as_integer("cold_htf_code");
        util::matrix_t<double> ud_CT_htf_props = as_matrix("ud_cold_htf_props");

        // *****************************************************
        // Power cycle
        double cycle_max_frac = as_double("cycle_max_frac");        //[-]
        double cycle_cutoff_frac = as_double("cycle_cutoff_frac");  //[-]
        double q_sby_frac = as_double("q_sby_frac");                //[-]
        double htf_pump_coef = as_double("pb_pump_coef");           //[kW/kg/s]
        double startup_time = as_double("startup_time");            //[hr]
        double startup_frac = as_double("startup_frac");            //[-]

        C_pc_ptes c_pc(W_dot_gen_thermo, eta_therm_mech,
            W_dot_gen_used_elec, fixed__q_dot_cold__to__q_dot_warm,
            T_HT_hot_TES, T_HT_cold_TES, T_CT_cold_TES, T_CT_hot_TES,
            cycle_max_frac, cycle_cutoff_frac, q_sby_frac,
            startup_time, startup_frac,
            htf_pump_coef,
            HT_htf_code, ud_HT_htf_props,
            CT_htf_code, ud_CT_htf_props);

        C_csp_power_cycle::S_solved_params pc_solved_params;
        
        try {
            c_pc.init(pc_solved_params);
        }
        catch (C_csp_exception& csp_exception) {

            int out_type = -1;
            std::string out_msg = "";
            // Report warning before exiting with error
            while (c_pc.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg, out_type);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }
        // **********************************************************
        // **********************************************************

        // **********************************************************
        // Heat pump
        double f_q_dot_des_allowable_su = as_double("f_q_dot_des_allowable_su");    //[-] fraction of design power allowed during startup
        double hrs_startup_at_max_rate = as_double("hrs_startup_at_max_rate");      //[hr] duration of startup at max startup power
        double f_heater_min = as_double("f_q_dot_heater_min");                      //[-] minimum allowable heater output as fraction of design

        double heat_pump_HT_htf_pump_coef = as_double("heat_pump_HT_HTF_pump_coef");           //[kW/kg/s]
        double heat_pump_CT_htf_pump_coef = as_double("heat_pump_CT_HTF_pump_coef");           //[kW/kg/s]

        C_csp_cr_heat_pump c_heat_pump(COP_heat_charge_therm, q_dot_hot_out_charge,
            f_elec_consume_vs_W_dot_thermo,
            T_HT_hot_TES, T_HT_cold_TES, T_CT_cold_TES, T_CT_hot_TES,
            f_heater_min, f_q_dot_des_allowable_su, hrs_startup_at_max_rate,
            heat_pump_HT_htf_pump_coef, heat_pump_CT_htf_pump_coef,
            HT_htf_code, ud_HT_htf_props,
            CT_htf_code, ud_CT_htf_props);

        // **********************************************************
        // **********************************************************

        // **********************************************************
        // High temp TES
        C_csp_two_tank_tes c_HT_TES;
        {
            C_csp_two_tank_tes::S_params* tes = &c_HT_TES.ms_params;
            tes->m_field_fl = HT_htf_code;
            tes->m_field_fl_props = ud_HT_htf_props;
            tes->m_tes_fl = HT_htf_code;
            tes->m_tes_fl_props = ud_HT_htf_props;
            tes->m_W_dot_pc_design = W_dot_gen_thermo;  //[MWe]
            tes->m_eta_pc = eta_therm_mech;             //[-]
            tes->m_solarm = heater_mult;                //[-]
            tes->m_ts_hours = tshours;                  //[hr]
            tes->m_h_tank = as_double("h_tank");
            tes->m_u_tank = as_double("u_tank");
            tes->m_tank_pairs = as_integer("tank_pairs");
            tes->m_hot_tank_Thtr = as_double("hot_tank_Thtr");
            tes->m_hot_tank_max_heat = as_double("hot_tank_max_heat");
            tes->m_cold_tank_Thtr = as_double("cold_tank_Thtr");
            tes->m_cold_tank_max_heat = as_double("cold_tank_max_heat");
            tes->m_dt_hot = 0.0;                        // MSPT assumes direct storage, so no user input here: hardcode = 0.0
            tes->m_T_field_in_des = T_HT_cold_TES;      //[C]
            tes->m_T_field_out_des = T_HT_hot_TES;      //[C]
            tes->m_T_tank_hot_ini = T_HT_hot_TES;       //[C]
            tes->m_T_tank_cold_ini = T_HT_cold_TES;     //[C]
            tes->m_h_tank_min = as_double("h_tank_min");
            tes->m_f_V_hot_ini = as_double("tes_init_hot_htf_percent");
            tes->m_htf_pump_coef = 0.0;         //[kW/kg/s] No htf pump losses in direct TES
            tes->tanks_in_parallel = false;     //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
            tes->V_tes_des = 1.85;              //[m/s]
            tes->calc_design_pipe_vals = false; // for now, to get 'tanks_in_parallel' to work
        }

        // **********************************************************
        // **********************************************************

        // **********************************************************
        // Cold temp TES
        C_csp_two_tank_tes c_CT_TES;
        {
            C_csp_two_tank_tes::S_params* ctes = &c_CT_TES.ms_params;
            ctes->m_field_fl = CT_htf_code;
            ctes->m_field_fl_props = ud_CT_htf_props;
            ctes->m_tes_fl = CT_htf_code;
            ctes->m_tes_fl_props = ud_CT_htf_props;
            // Power/effiency relationship doesn't hold for cold tank, so just fake it with power = heat and eta = 1
            ctes->m_W_dot_pc_design = q_dot_cold_in_charge;     //[MWt]
            ctes->m_eta_pc = 1.0;                               //[-]
                // *************************************************
            ctes->m_solarm = heater_mult;                //[-]
            ctes->m_ts_hours = tshours;                  //[hr]
            ctes->m_h_tank = as_double("CT_h_tank");
            ctes->m_u_tank = as_double("CT_u_tank");
            ctes->m_tank_pairs = as_integer("CT_tank_pairs");
            // If CTES is colder than ambient, then heaters aren't going to do much?
            ctes->m_hot_tank_Thtr = -200.0;         //[C]
            ctes->m_hot_tank_max_heat = 0.0;        //[MWt]
            ctes->m_cold_tank_Thtr = -200.0;        //[C]
            ctes->m_cold_tank_max_heat = 0.0;       //[MWt]
                // so do we want these inputs from cmod?
                // do we need a tank cooler? Close enough to ambient (we expect?) to have minor heat loss?
                // ctes->m_hot_tank_Thtr = as_double("CT_hot_tank_Thtr");
                // ctes->m_hot_tank_max_heat = as_double("CT_hot_tank_max_heat");
                // ctes->m_cold_tank_Thtr = as_double("CT_cold_tank_Thtr");
                // ctes->m_cold_tank_max_heat = as_double("CT_cold_tank_max_heat");
            // ********************************************************************
            ctes->m_dt_hot = 0.0;                        // MSPT assumes direct storage, so no user input here: hardcode = 0.0
            ctes->m_T_field_in_des = T_CT_cold_TES;      //[C]
            ctes->m_T_field_out_des = T_CT_hot_TES;      //[C]
            ctes->m_T_tank_hot_ini = T_CT_hot_TES;       //[C]
            ctes->m_T_tank_cold_ini = T_CT_cold_TES;     //[C]
            ctes->m_h_tank_min = as_double("CT_h_tank_min");
            ctes->m_f_V_hot_ini = as_double("CT_tes_init_hot_htf_percent");
            ctes->m_htf_pump_coef = 0.0;            //[kW/kg/s] No htf pump losses in direct TES
            ctes->tanks_in_parallel = false;        //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
            ctes->V_tes_des = 1.85;                 //[m/s]
            ctes->calc_design_pipe_vals = false;    // for now, to get 'tanks_in_parallel' to work
        }

        // **********************************************************
        // **********************************************************

        // **********************************************************


        return;
    }
};

DEFINE_MODULE_ENTRY(etes_ptes, "Heat pump charging two two-tank TES from grid, discharge with power cycle", 1)
