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
#include "csp_common.h"
#include "csp_solver_core.h"
#include "csp_solver_cr_electric_resistance.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_tou_block_schedules.h"

static var_info _cm_vtab_etes_electric_resistance[] = {

    // Resource Data
    { SSC_INPUT,  SSC_STRING, "solar_resource_file",           "Local weather file path",                                        "",             "",                                  "Solar Resource",                    "?",                                                                "LOCAL_FILE",    ""},

    // Simulation Parametes
    { SSC_INPUT,  SSC_NUMBER, "time_start",                    "Simulation start time",                                          "s",            "",                                  "System Control",                           "?=0",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "time_stop",                     "Simulation stop time",                                           "s",            "",                                  "System Control",                           "?=31536000",                                                       "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "time_steps_per_hour",           "Number of simulation time steps per hour",                       "",             "",                                  "System Control",                           "?=-1",                                                             "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "vacuum_arrays",                 "Allocate arrays for only the required number of steps",          "",             "",                                  "System Control",                           "?=0",                                                              "",              ""},


    // System 
    { SSC_INPUT,  SSC_NUMBER, "T_htf_cold_des",                "Cold HTF inlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_htf_hot_des",                 "Hot HTF outlet temperature at design conditions",                "C",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_ref",                         "Reference output electric power at design condition",            "MW",           "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "design_eff",                    "Power cycle efficiency at design",                               "none",         "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tshours",                       "Equivalent full-load thermal storage hours",                     "hr",           "",                                  "System Design",                            "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_NUMBER, "heater_mult",                   "Heater multiple relative to design cycle thermal power",         "-",            "",                                  "System Design",                            "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "gross_net_conversion_factor",   "Estimated gross to net conversion factor",                       "",             "",                                  "System Design",                            "*",                                                                "",              ""},


    // Power Cycle
        // General
    { SSC_INPUT,  SSC_NUMBER, "pc_config",                     "PC configuration 0=Steam Rankine, 1=user defined",               "",             "",                                  "Power Cycle",                              "?=0",                                                              "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "pb_pump_coef",                  "Pumping power to move 1kg of HTF through PB loop",               "kW/kg",        "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_time",                  "Time needed for power block startup",                            "hr",           "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "startup_frac",                  "Fraction of design thermal power needed for startup",            "none",         "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_max_frac",                "Maximum turbine over design operation fraction",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cycle_cutoff_frac",             "Minimum turbine operation fraction before shutdown",             "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "q_sby_frac",                    "Fraction of thermal power required for standby",                 "",             "",                                  "Power Cycle",                              "*",                                                                "",              ""},
        // Steam Rankine
    { SSC_INPUT,  SSC_NUMBER, "dT_cw_ref",                     "Reference condenser cooling water inlet/outlet temperature difference",  "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_amb_des",                     "Reference ambient temperature at design point",                          "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_boil",                        "Boiler operating pressure",                                              "bar",  "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "CT",                            "Condensor type: 1=evaporative, 2=air, 3=hybrid",                         "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_approach",                    "Cooling tower approach temperature",                                     "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "T_ITD_des",                     "ITD at design for dry system",                                           "C",    "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_cond_ratio",                  "Condenser pressure ratio",                                               "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "pb_bd_frac",                    "Power block blowdown steam fraction",                                    "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "P_cond_min",                    "Minimum condenser pressure",                                             "inHg", "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "n_pl_inc",                      "Number of part-load increments for the heat rejection system",           "none", "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "INTEGER",       ""},
    { SSC_INPUT,  SSC_ARRAY,  "F_wc",                          "TOU array of fractions indicating wet cooling share for hybrid cooling", "",     "",                                  "System Control",                           "pc_config=0",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tech_type",                     "Turbine inlet pressure control 1=Fixed, 3=Sliding",                      "",     "",                                  "Rankine Cycle",                            "pc_config=0",                                                      "",              ""},
        // User Defined cycle
    { SSC_INPUT,  SSC_NUMBER, "ud_f_W_dot_cool_des",           "Percent of user-defined power cycle design gross output consumed by cooling",                     "%",      "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "ud_m_dot_water_cool_des",       "Mass flow rate of water required at user-defined power cycle design point",                       "kg/s",   "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "ud_ind_od",                     "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb", "",       "",       "User Defined Power Cycle",                 "pc_config=1",                                                      "",              ""},


    // TES 
/*new*/    { SSC_INPUT,  SSC_NUMBER, "tes_fl_code",                   "Receiver HTF, 17=Salt (60% NaNO3, 40% KNO3) 10=Salt (46.5% LiF 11.5% NaF 42% KF) 50=Lookup tables", "", "",           "Thermal Storage",                                      "*",                                                                "",              ""},
/*new*/    { SSC_INPUT,  SSC_MATRIX, "ud_tes_fl_props",               "User-defined TES fluid property data",                           "-",            "",                                  "Thermal Storage",                                      "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "csp.pt.tes.init_hot_htf_percent", "Initial fraction of available volume that is hot",              "%",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank",                        "Total height of tank (height of HTF when tank is full)",        "m",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_max_heat",            "Rated heater capacity for cold tank heating",                   "MW",           "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "u_tank",                        "Loss coefficient from the tank",                                "W/m2-K",       "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "tank_pairs",                    "Number of equivalent tank pairs",                               "",             "",                                  "Thermal Storage",                          "*",                                                                "INTEGER",       ""},
    { SSC_INPUT,  SSC_NUMBER, "cold_tank_Thtr",                "Minimum allowable cold tank HTF temperature",                   "C",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "h_tank_min",                    "Minimum allowable HTF height in storage tank",                  "m",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_Thtr",                 "Minimum allowable hot tank HTF temperature",                    "C",            "",                                  "Thermal Storage",                          "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "hot_tank_max_heat",             "Rated heater capacity for hot tank heating",                    "MW",           "",                                  "Thermal Storage",                          "*",                                                                "",              ""},


    // System control
    { SSC_INPUT,  SSC_NUMBER, "is_dispatch",                   "Allow dispatch optimization?",                                  "",             "",                                  "System Control",                           "?=0",                                                              "",              ""},


    // System performance
    { SSC_INPUT,  SSC_NUMBER, "pb_fixed_par",                  "Fixed parasitic load that don't generate heat - runs at all times","MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par",                       "Balance of plant parasitic power fraction",                        "MWe/MWcap", "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_f",                     "Balance of plant parasitic power fraction - mult frac",            "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_0",                     "Balance of plant parasitic power fraction - const coeff",          "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_1",                     "Balance of plant parasitic power fraction - linear coeff",         "",          "",                                  "System Control",                           "*",                                                                "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "bop_par_2",                     "Balance of plant parasitic power fraction - quadratic coeff",      "",          "",                                  "System Control",                           "*",                                                                "",              ""},


    // Pricing schedules and multipliers
    { SSC_INPUT,  SSC_NUMBER, "ppa_multiplier_model",          "PPA multiplier model",                                          "0/1",          "0=diurnal,1=timestep",              "Time of Delivery Factors",                 "?=0",                                                              "INTEGER,MIN=0", ""},
    { SSC_INPUT,  SSC_ARRAY,  "dispatch_factors_ts",           "Dispatch payment factor array",                                 "",             "",                                  "Time of Delivery Factors",                 "ppa_multiplier_model=1",                                           "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekday",        "PPA pricing weekday schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "?=[[1]]",                                                          "",              ""},
    { SSC_INPUT,  SSC_MATRIX, "dispatch_sched_weekend",        "PPA pricing weekend schedule, 12x24",                           "",             "",                                  "Time of Delivery Factors",                 "?=[[1]]",                                                          "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor1",              "Dispatch payment factor 1",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor2",              "Dispatch payment factor 2",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor3",              "Dispatch payment factor 3",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor4",              "Dispatch payment factor 4",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor5",              "Dispatch payment factor 5",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor6",              "Dispatch payment factor 6",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor7",              "Dispatch payment factor 7",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor8",              "Dispatch payment factor 8",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},
    { SSC_INPUT,  SSC_NUMBER, "dispatch_factor9",              "Dispatch payment factor 9",                                     "",             "",                                  "Time of Delivery Factors",                 "?=1",                                                              "",              ""},


    // ****************************************************************************************************************************************
    // Outputs here:
    // ****************************************************************************************************************************************
        // Simulation outputs
    { SSC_OUTPUT, SSC_ARRAY,  "time_hr",                       "Time at end of timestep",                                       "hr",           "",                                  "",                                         "*",                                                                "",              ""},

        // Heater outputs
    { SSC_OUTPUT, SSC_ARRAY,  "W_dot_heater",                  "Electricity consumed by heater",                                "MWe",          "",                                  "",                                         "*",                                                                "",              ""},

        // Cycle outputs
    { SSC_OUTPUT, SSC_ARRAY,  "P_cycle_gross",                 "PC electrical power gross (no cooling parasitics)",                                                "MWe",          "",               "powerblock",     "*",                       "",                      "" },


        // System outputs
    { SSC_OUTPUT, SSC_ARRAY,  "P_out_net",                     "Total electric power to grid",                                  "MWe",          "",                                  "",                                         "*",                                                                "",              ""},

    { SSC_OUTPUT, SSC_ARRAY,  "gen",                           "Total electric power to grid with available derate",            "kWe",          "",                                  "",                                         "*",                                                                "",              "" },


        // Annual single-value outputs
    { SSC_OUTPUT, SSC_NUMBER, "annual_energy",                 "Annual total electric power to grid",                           "kWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_E_heater",               "Annual electric energy consumed by heater",                     "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },
    { SSC_OUTPUT, SSC_NUMBER, "annual_E_cycle_gross",          "Annual gross electric energy generated by cycle",               "MWhe",         "",                                  "",                                         "*",                                                                "",              "" },


    var_info_invalid };

class cm_etes_electric_resistance : public compute_module
{
public:

    cm_etes_electric_resistance()
    {
        add_var_info(_cm_vtab_etes_electric_resistance);
        add_var_info(vtab_adjustment_factors);
    }

    void exec() override
    {
        // *****************************************************
        // System Design Parameters
        double T_htf_cold_des = as_double("T_htf_cold_des");    //[C]
        double T_htf_hot_des = as_double("T_htf_hot_des");      //[C]
        double W_dot_cycle_des = as_double("P_ref");            //[MWe]
        double eta_cycle = as_double("design_eff");             //[-]
        double tshours = as_double("tshours");                  //[-]
        double heater_mult = as_double("heater_mult");          //[-]
        double gross_net_conversion_factor = as_double("gross_net_conversion_factor");

        // TES parameters
        int tes_fl_code = as_integer("tes_fl_code");
        util::matrix_t<double> ud_tes_fl_props = as_matrix("ud_tes_fl_props");

        // System Design Calcs
        double q_dot_pc_des = W_dot_cycle_des / eta_cycle;      //[MWt]
        double q_dot_heater_des = q_dot_pc_des * heater_mult;   //[MWt]
        double system_capacity = W_dot_cycle_des * gross_net_conversion_factor * 1.E-3; //[kWe]
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
        if (weather_reader.has_error()) throw exec_error("tcsmolten_salt", weather_reader.get_error());
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
                throw spexception("The time step duration must be evenly divisible within an hour.");
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
        // Steam Rankine and User Defined power cycle classes
        C_pc_Rankine_indirect_224 rankine_pc;

        int pb_tech_type = as_integer("pc_config");
        if (pb_tech_type == 0 || pb_tech_type == 1)
        {
            C_pc_Rankine_indirect_224::S_params* pc = &rankine_pc.ms_params;
            pc->m_P_ref = W_dot_cycle_des;              //[MWe]
            pc->m_eta_ref = eta_cycle;                  //[-]
            pc->m_T_htf_hot_ref = T_htf_hot_des;        //[C]
            pc->m_T_htf_cold_ref = T_htf_cold_des;      //[C]
            pc->m_cycle_max_frac = as_double("cycle_max_frac");
            pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
            pc->m_q_sby_frac = as_double("q_sby_frac");
            pc->m_startup_time = as_double("startup_time");
            pc->m_startup_frac = as_double("startup_frac");
            pc->m_htf_pump_coef = as_double("pb_pump_coef");
            pc->m_pc_fl = tes_fl_code;      
            pc->m_pc_fl_props = ud_tes_fl_props;

            if (pb_tech_type == 0)
            {
                pc->m_dT_cw_ref = as_double("dT_cw_ref");
                pc->m_T_amb_des = as_double("T_amb_des");
                pc->m_P_boil = as_double("P_boil");
                pc->m_CT = as_integer("CT");                    // cooling tech type: 1=evaporative, 2=air, 3=hybrid
                if (pc->m_CT > 3) {
                    std::string err_msg = util::format("The specified power cycle cooling tech type, %d, does not exist"
                        " for the ETES electric resistance heating model. Choose from 1) evaporative, 2) air, or 3) hyrbid\n", pb_tech_type);
                    log(err_msg, SSC_WARNING);
                    return;
                }
                pc->m_tech_type = as_integer("tech_type");      // 1: Fixed, 3: Sliding
                if (pc->m_tech_type == 2) { pc->m_tech_type = 1; }; // changing fixed pressure for the trough to fixed pressure for the tower

                if (!(pc->m_tech_type == 1 || pc->m_tech_type == 3 || pc->m_tech_type == 5 || pc->m_tech_type == 6 || pc->m_tech_type == 7 || pc->m_tech_type == 8))
                {
                    std::string tech_msg = util::format("tech_type must be either 1 (fixed pressure) or 3 (sliding). Input was %d."
                        " Simulation proceeded with fixed pressure", pc->m_tech_type);
                    pc->m_tech_type = 1;
                }
                pc->m_T_approach = as_double("T_approach");
                pc->m_T_ITD_des = as_double("T_ITD_des");
                pc->m_P_cond_ratio = as_double("P_cond_ratio");
                pc->m_pb_bd_frac = as_double("pb_bd_frac");
                pc->m_P_cond_min = as_double("P_cond_min");
                pc->m_n_pl_inc = as_integer("n_pl_inc");

                size_t n_F_wc = 0;
                ssc_number_t* p_F_wc = as_array("F_wc", &n_F_wc);
                pc->m_F_wc.resize(n_F_wc, 0.0);
                for (size_t i = 0; i < n_F_wc; i++)
                    pc->m_F_wc[i] = (double)p_F_wc[i];

                // Set User Defined cycle parameters to appropriate values
                pc->m_is_user_defined_pc = false;
                pc->m_W_dot_cooling_des = std::numeric_limits<double>::quiet_NaN();
            }
            else if (pb_tech_type == 1)
            {
                pc->m_is_user_defined_pc = true;

                // User-Defined Cycle Parameters
                pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0 * as_double("P_ref");  //[MWe]
                pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");       //[kg/s]

                // User-Defined Cycle Off-Design Tables 
                pc->mc_combined_ind = as_matrix("ud_ind_od");
            }
        }
        else
        {
            std::string err_msg = util::format("The specified power cycle configuration, %d, does not exist. See SSC Input Table for help.\n", pb_tech_type);
            log(err_msg, SSC_WARNING);
            return;
        }

        // Set cycle cmod outputs
        rankine_pc.assign(C_pc_Rankine_indirect_224::E_W_DOT, allocate("P_cycle_gross", n_steps_fixed), n_steps_fixed);


        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Electric resistance heater
        // Construct electric resistance heater class
        double f_q_dot_des_allowable_su = 1.0;  //[-]
        double hrs_startup_at_max_rate = 0.25;  //[hr]
        C_csp_cr_electric_resistance c_electric_resistance(T_htf_cold_des, T_htf_hot_des, q_dot_heater_des,
            f_q_dot_des_allowable_su, hrs_startup_at_max_rate,
            tes_fl_code, ud_tes_fl_props);

        // Test init()
        //C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
        //C_csp_collector_receiver::S_csp_cr_solved_params solved_params;
        //c_electric_resistance.init(init_inputs, solved_params);

        // Set heater cmod outputs
        c_electric_resistance.mc_reported_outputs.assign(C_csp_cr_electric_resistance::E_W_DOT_HEATER, allocate("W_dot_heater", n_steps_fixed), n_steps_fixed);

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // TES
        C_csp_two_tank_tes storage;
        C_csp_two_tank_tes::S_params* tes = &storage.ms_params;
        tes->m_field_fl = tes_fl_code;
        tes->m_field_fl_props = ud_tes_fl_props;
        tes->m_tes_fl = tes_fl_code;
        tes->m_tes_fl_props = ud_tes_fl_props;
        tes->m_is_hx = false;                       // ETES assumes direct storage, so no user input required here: hardcode = false
        tes->m_W_dot_pc_design = W_dot_cycle_des;   //[MWe]
        tes->m_eta_pc = eta_cycle;                  //[-]
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
        tes->m_T_field_in_des = T_htf_cold_des;     //[C]
        tes->m_T_field_out_des = T_htf_hot_des;     //[C]
        tes->m_T_tank_hot_ini = T_htf_hot_des;      //[C]
        tes->m_T_tank_cold_ini = T_htf_cold_des;    //[C]
        tes->m_h_tank_min = as_double("h_tank_min");
        tes->m_f_V_hot_ini = as_double("csp.pt.tes.init_hot_htf_percent");
        tes->m_htf_pump_coef = as_double("pb_pump_coef");
        tes->tanks_in_parallel = false;     //[-] False: Field HTF always goes to TES. PC HTF always comes from TES. ETES should not simultaneously operate heater and cycle
        tes->V_tes_des = 1.85;              //[m/s]
        tes->calc_design_pipe_vals = false; // for now, to get 'tanks_in_parallel' to work

        // Set TES cmod outputs

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Pricing and operation schedules
        C_csp_tou_block_schedules tou;
        C_csp_tou_block_schedules::S_params* tou_params = &tou.ms_params;

        // Still need to define mc_csp_ops blocks and fractions although we're not using them
        tou_params->mc_csp_ops.mc_weekdays.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mc_weekends.resize_fill(12, 24, 1.0);
        tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(2, std::numeric_limits<double>::quiet_NaN());

        tou_params->mc_pricing.mc_weekdays = as_matrix("dispatch_sched_weekday");
        if (tou_params->mc_pricing.mc_weekdays.ncells() == 1) { tou_params->mc_pricing.mc_weekdays.resize_fill(12, 24, 1.); };
        tou_params->mc_pricing.mc_weekends = as_matrix("dispatch_sched_weekend");
        if (tou_params->mc_pricing.mc_weekends.ncells() == 1) { tou_params->mc_pricing.mc_weekends.resize_fill(12, 24, 1.); };
        tou.mc_dispatch_params.m_dispatch_optimize = as_boolean("is_dispatch");

        tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max = true;
        tou.mc_dispatch_params.m_is_block_dispatch = false;
        tou.mc_dispatch_params.m_is_arbitrage_policy = !tou.mc_dispatch_params.m_dispatch_optimize;
        tou.mc_dispatch_params.m_use_rule_1 = false;
        tou.mc_dispatch_params.m_standby_off_buffer = 2.0;          //[hr] Applies if m_use_rule_1 is true
        tou.mc_dispatch_params.m_use_rule_2 = false;
        tou.mc_dispatch_params.m_q_dot_rec_des_mult = -1.23;        //[-] Applies if m_use_rule_2 is true
        tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;      //[-] Applies if m_use_rule_2 is true

        bool is_timestep_input = (as_integer("ppa_multiplier_model") == 1);
        tou_params->mc_pricing.mv_is_diurnal = !(is_timestep_input);
        if (is_timestep_input)
        {
            size_t nmultipliers;
            ssc_number_t* multipliers = as_array("dispatch_factors_ts", &nmultipliers);
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(nmultipliers, 0.0);
            for (size_t ii = 0; ii < nmultipliers; ii++)
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = multipliers[ii];
        }
        else // standard diuranal input
        {
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = as_double("dispatch_factor1");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = as_double("dispatch_factor2");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = as_double("dispatch_factor3");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = as_double("dispatch_factor4");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = as_double("dispatch_factor5");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = as_double("dispatch_factor6");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = as_double("dispatch_factor7");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = as_double("dispatch_factor8");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = as_double("dispatch_factor9");
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // System performance
        C_csp_solver::S_csp_system_params system;
        system.m_pb_fixed_par = as_double("pb_fixed_par");
        system.m_bop_par = as_double("bop_par");
        system.m_bop_par_f = as_double("bop_par_f");
        system.m_bop_par_0 = as_double("bop_par_0");
        system.m_bop_par_1 = as_double("bop_par_1");
        system.m_bop_par_2 = as_double("bop_par_2");

        // *****************************************************
        // *****************************************************

        // *****************************************************
        // Construct System Simulation
        C_csp_solver csp_solver(weather_reader,
            c_electric_resistance,
            rankine_pc,
            storage,
            tou,
            system,
            ssc_cmod_update,
            (void*)(this));

        // Set system cmod outputs
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TIME_FINAL, allocate("time_hr", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::W_DOT_NET, allocate("P_out_net", n_steps_fixed), n_steps_fixed);

        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Initialize
        update("Initialize ETES electric resistance heating model...", 0.0);

        int out_type = -1;
        std::string out_msg = "";
        try
        {
            // Initialize Solver
            csp_solver.init();
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg, out_type);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Run timeseries simulation
        update("Begin timeseries simulation...", 0.0);

        try
        {
            // Simulate !
            csp_solver.Ssimulate(sim_setup);
        }
        catch (C_csp_exception& csp_exception)
        {
            // Report warning before exiting with error
            while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
            {
                log(out_msg);
            }

            throw exec_error("etes_electric_resistance", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Post-process
        size_t count;
        ssc_number_t* p_W_dot_net = as_array("P_out_net", &count);
        ssc_number_t* p_time_final_hr = as_array("time_hr", &count);

        // 'adjustment_factors' class stores factors in hourly array, so need to index as such
        adjustment_factors haf(this, "adjust");
        if (!haf.setup())
            throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());

        ssc_number_t* p_gen = allocate("gen", count);
        for (size_t i = 0; i < count; i++)
        {
            size_t hour = (size_t)ceil(p_time_final_hr[i]);
            p_gen[i] = (ssc_number_t)(p_W_dot_net[i] * 1.E3 * haf(hour));           //[kWe]
        }
        // *****************************************************
        // *****************************************************


        // *****************************************************
        // Report simulation metrics
        ssc_number_t* p_annual_energy_dist_time = gen_heatmap(this, steps_per_hour);

        accumulate_annual_for_year("gen", "annual_energy", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("W_dot_heater", "annual_E_heater", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWhe]
        accumulate_annual_for_year("P_cycle_gross", "annual_E_cycle_gross", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[MWhe]


    }
};

DEFINE_MODULE_ENTRY(etes_electric_resistance, "Electric resistance heater charging TES from grid, discharge with power cycle", 1)
