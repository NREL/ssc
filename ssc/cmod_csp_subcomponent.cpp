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
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_NTHeatTrap_tes.h"
#include "csp_solver_particlecline_tes.h"

// Forward declarations
double C_to_K(double T);
double K_to_C(double T);

static var_info _cm_vtab_csp_subcomponent[] = {
    /* VARTYPE          DATATYPE         NAME                         LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS         UI_HINTS*/
    // Inputs
    { SSC_INPUT,        SSC_NUMBER,      "t_step",                    "Timestep duration",                                                                "s",            "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "T_amb",                     "Ambient temperature",                                                              "C",            "",               "weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "mdot_src",                  "Mass flow from heat source",                                                       "kg/s",         "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "mdot_sink",                 "Mass flow to heat sink or power block",                                            "kg/s",         "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "hot_tank_bypassed",         "Is mass flow from source going straight to cold tank?",                            "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "T_src_out",                 "Temperature from heat source",                                                     "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "T_sink_out",                "Temperature from heat sink or power block",                                        "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",            "Temperature of fluid in hot tank at beginning of step",                            "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Temperature of fluid in cold tank at beginning of step",                           "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_xsteps",                  "Number of spatial segments",                                                       "",             "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_tsteps",                  "Number of subtimesteps",                                                           "",             "",               "TES",            "*",                       "",                      "" },


    // TES
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",                     "Field HTF fluid ID number",                                                        "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",            "User defined field fluid property data",                                           "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                                                "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                                         "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                     "Rated plant capacity",                                                             "MWe",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",                   "Power cycle efficiency at design",                                                 "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",                "Actual solar multiple of system",                                                  "-",            "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                                       "hr",           "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",                            "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                                   "W/m2-K",       "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                                  "-",            "",               "TES",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                                              "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",         "Rated heater capacity for hot tank heating",                                       "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                                             "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",        "Rated heater capacity for cold tank heating",                                      "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                                        "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "Design loop inlet temperature",                                                    "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "Target loop outlet temperature",                                                   "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                                     "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",      "Initial fraction of avail. vol that is hot",                                       "%",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",              "Pumping power to move 1kg of HTF through PB loop",                                 "kW/kg",        "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tanks_in_parallel",         "Tanks are in parallel, not in series, with solar field",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tes_des",                 "Design-point velocity to size the TES pipe diameters",                             "m/s",          "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "calc_design_pipe_vals",     "Calculate temps and pressures at design conditions for runners and headers",       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",                                "kW/(kg/s)",    "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "has_hot_tank_bypass",       "Bypass valve connects field outlet to cold tank",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_inlet_min",      "Minimum hot tank htf inlet temperature",                                           "C",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_p_loss",         "TES pipe losses are based on custom lengths and coeffs",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_pipe_sizes",     "Use custom TES pipe diams, wallthks, and lengths",                                 "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "k_tes_loss_coeffs",         "Minor loss coeffs for the coll, gen, and bypass loops",                            "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_diams",                 "Custom TES diameters",                                                             "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_wallthicks",            "Custom TES wall thicknesses",                                                      "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_lengths",               "Custom TES lengths",                                                               "m",            "",               "controller",     "",                        "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DP_SGS",                    "Pressure drop within the steam generator",                                         "bar",          "",               "controller",     "*",                       "",                      "" },

    // Added Inputs for NT System
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",                  "Standard two tank (0), HeatTrap Single Tank (1)",                                  "-",            "",               "TES",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_tank_thick",            "Tank wall thickness (used for Norwich HeatTrap)",                                  "m",            "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_tank_cp",               "Tank wall cp (used for Norwich HeatTrap)",                                         "kJ/kg-K",      "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_tank_dens",             "Tank wall thickness (used for Norwich HeatTrap)",                                  "kg/m3",        "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_NT_nstep",              "Number of time steps for energy balance (used for Norwich HeatTrap)",              "",             "",               "TES",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tes_NT_piston_loss_poly",   "Polynomial coefficients describing piston heat loss function (f(kg/s)=%)",         "",             "",               "TES",            "tes_type=1",              "",                      "" },


    // Particle Specific Inputs
    { SSC_INPUT,        SSC_ARRAY,       "T_grad_ini",                "TES Temperature gradient at beginning of timestep",                                "C",            "",               "TES",            "?=[-274]",              "",                      "" },


    // Outputs
    { SSC_OUTPUT,       SSC_ARRAY,       "T_src_in",                  "Temperature to heat source",                                                       "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_sink_in",                 "Temperature to heat sink or power block",                                          "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold",               "Temperature of cold tank (average)",                                               "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot",                "Temperature of hot tank (average)",                                                "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hot_tank_vol_frac",         "Hot tank volume fraction of total",                                                "",             "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_elec_in_tot",         "TES power consumed",                                                               "MWe",          "",               "TES",            "tes_type=2",              "",                      "" },


    { SSC_OUTPUT,       SSC_ARRAY,       "tes_error",                 "TES energy balance error",                                                         "MW",           "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_error_percent",         "TES energy balance error percent",                                                 "%",            "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "piston_loc",                "Piston Location (distance from left cold side)",                                   "m",            "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "piston_frac",               "Piston Fraction (distance from left cold side)",                                   "",             "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_hot_calc",                "Analytical Hot Side Temperature (no losses)",                                      "C",            "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_cold_calc",               "Analytical Cold Side Temperature (no losses)",                                     "C",            "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_hot_error",             "TES hot energy balance error",                                                     "MWt",          "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_cold_error",            "TES cold energy balance error",                                                    "MWt",          "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_leak_error",            "TES energy balance error due to leakage assumption",                               "MWt",          "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_E_hot",                 "TES hot side internal energy",                                                     "MJ",           "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_E_cold",                "TES cold side internal energy",                                                    "MJ",           "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_wall_error",            "TES energy balance error due to wall temperature assumption",                      "MWt",          "",               "TES",            "tes_type=1",              "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tes_error_corrected",       "TES energy balance error, accounting for wall and temperature assumption error",   "MWt",          "",               "TES",            "tes_type=1",              "",                      "" },
                                                                                                                                                                                            
    { SSC_OUTPUT,       SSC_MATRIX,      "T_grad_final",              "TES Temperature gradient at end of timestep",                                      "C",            "",               "TES",            "tes_type=2",              "",                      "" },


    var_info_invalid };

class cm_csp_subcomponent : public compute_module
{
public:
    cm_csp_subcomponent()
    {
        add_var_info(_cm_vtab_csp_subcomponent);
    }

    void exec()
    {
        int tes_type = as_integer("tes_type");

        double_vec hot = as_vector_double("T_src_out");
        double_vec cold = as_vector_double("T_sink_out");
        double hot_des = as_double("T_loop_out");
        double cold_des = as_double("T_loop_in_des");
        

        util::matrix_t<double> tes_lengths;
        if (is_assigned("tes_lengths")) {
            tes_lengths = as_matrix("tes_lengths");               //[m]
        }
        if (!is_assigned("tes_lengths") || tes_lengths.ncells() < 11) {
            double vals1[11] = { 0., 90., 100., 120., 0., 30., 90., 80., 80., 120., 80. };
            tes_lengths.assign(vals1, 11);
        }


        C_csp_tes* storage_pointer;
        C_csp_two_tank_tes storage_two_tank;
        C_csp_NTHeatTrap_tes storage_NT;
        C_csp_particlecline_tes storage_particle;

        double P_ref = as_double("P_ref");
        double eta_ref = as_double("eta_ref");
        double tshours = as_double("tshours");

        // Two Tank
        if (tes_type == 0)
        {
            storage_two_tank = C_csp_two_tank_tes(
                as_integer("Fluid"),                                                // [-] field fluid identifier
                as_matrix("field_fl_props"),                                        // [-] field fluid properties
                as_integer("store_fluid"),                                          // [-] tes fluid identifier
                as_matrix("store_fl_props"),                                        // [-] tes fluid properties
                as_double("P_ref") / as_double("eta_ref"),                          // [MWt] Design heat rate in and out of tes
                as_double("solar_mult"),                                            // [-] the max design heat rate as a fraction of the nominal
                as_double("P_ref") / as_double("eta_ref") * as_double("tshours"),   // [MWt-hr] design storage capacity
                true,                                                               // Use input height
                as_double("h_tank"),                                                // [m] tank height input
                0.0,                                                                // [m] tank diameter input
                as_double("u_tank"),                                                // [W/m^2-K]
                as_integer("tank_pairs"),                                           // [-]
                as_double("hot_tank_Thtr"),                                         // [C] convert to K in init()
                as_double("hot_tank_max_heat"),                                     // [MW]
                as_double("cold_tank_Thtr"),                                        // [C] convert to K in init()
                as_double("cold_tank_max_heat"),                                    // [MW]
                as_double("dt_hot"),                                                // [C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
                as_double("T_loop_in_des"),                                         // [C] convert to K in init()
                as_double("T_loop_out"),                                            // [C] convert to K in init()
                as_double("T_tank_hot_ini"),                                        // [C] Initial temperature in hot storage tank
                as_double("T_tank_cold_ini"),                                       // [C] Initial temperature in cold storage cold
                as_double("h_tank_min"),                                            // [m] Minimum allowable HTF height in storage tank
                as_double("init_hot_htf_percent"),                                  // [%] Initial fraction of available volume that is hot
                as_double("pb_pump_coef"),                                          // [kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle
                as_boolean("tanks_in_parallel"),                                    // [-] Whether the tanks are in series or parallel with the solar field. Series means field htf must go through storage tanks.
                as_double("V_tes_des"),                                             // [m/s] Design-point velocity for sizing the diameters of the TES piping
                as_boolean("calc_design_pipe_vals"),                                // [-] Should the HTF state be calculated at design conditions
                as_double("tes_pump_coef"),                                         // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
                as_double("eta_pump"),                                              // [-] Pump efficiency, for newer pumping calculations
                as_boolean("has_hot_tank_bypass"),                                  // [-] True if the bypass valve causes the field htf to bypass just the hot tank and enter the cold tank before flowing back to the field.
                as_double("T_tank_hot_inlet_min"),                                  // [C] Minimum field htf temperature that may enter the hot tank
                as_boolean("custom_tes_p_loss"),                                    // [-] True if the TES piping losses should be calculated using the TES pipe lengths and minor loss coeffs, false if using the pumping loss parameters
                as_boolean("custom_tes_pipe_sizes"),                                // [-] True if the TES diameters and wall thicknesses parameters should be used instead of calculating them
                as_matrix("k_tes_loss_coeffs"),                                     // [-] Combined minor loss coefficients of the fittings and valves in the collection (including bypass) and generation loops in the TES 
                as_matrix("tes_diams"),                                             // [m] Imported inner diameters for the TES piping as read from the modified output files
                as_matrix("tes_wallthicks"),                                        // [m] Imported wall thicknesses for the TES piping as read from the modified output files
                tes_lengths,                                                        // [m] Imported lengths for the TES piping as read from the modified output files
                as_double("HDR_rough"),                                             // [m] Pipe absolute roughness
                as_double("DP_SGS")                                                 // [bar] Pressure drop on the TES discharge side (e.g., within the steam generator)
            );

            storage_pointer = &storage_two_tank;
        }
        // Norwich HeatTrap
        else if (tes_type == 1)
        {

            int nstep = as_integer("tes_NT_nstep");

            bool custom_tes_pipe_sizes = as_boolean("custom_tes_pipe_sizes");
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

            bool tanks_in_parallel = as_boolean("tanks_in_parallel");
            if (tanks_in_parallel == false)
            {
                throw exec_error("csp_subcomponent", "TES model requires tanks in parallel");
            }

            storage_NT = C_csp_NTHeatTrap_tes(
                as_integer("Fluid"),                                                // [-] field fluid identifier
                as_matrix("field_fl_props"),                                        // [-] field fluid properties
                as_integer("store_fluid"),                                          // [-] tes fluid identifier
                as_matrix("store_fl_props"),                                        // [-] tes fluid properties
                as_double("P_ref") / as_double("eta_ref"),                          // [MWt] Design heat rate in and out of tes
                as_double("solar_mult"),                                            // [-] the max design heat rate as a fraction of the nominal
                as_double("P_ref") / as_double("eta_ref") * as_double("tshours"),   // [MWt-hr] design storage capacity
                true,   // use input height                                         // Use input height
                as_double("h_tank"),                                                // [m] tank height input
                0.0,    // no input diameter                                        // [m] tank diameter input
                as_double("u_tank"),                                                // [W/m^2-K]
                as_integer("tank_pairs"),                                           // [-]
                as_double("hot_tank_Thtr"),                                         // [C] convert to K in init()
                as_double("hot_tank_max_heat"),                                     // [MW]
                as_double("cold_tank_Thtr"),                                        // [C] convert to K in init()
                as_double("cold_tank_max_heat"),                                    // [MW]
                as_double("dt_hot"),                                                // [C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
                as_double("T_loop_in_des"),                                         // [C] convert to K in init()
                as_double("T_loop_out"),                                            // [C] convert to K in init()
                as_double("T_tank_hot_ini"),                                        // [C] Initial temperature in hot storage tank
                as_double("T_tank_cold_ini"),                                       // [C] Initial temperature in cold storage cold
                as_double("h_tank_min"),                                            // [m] Minimum allowable HTF height in storage tank
                as_double("init_hot_htf_percent"),                                  // [%] Initial fraction of available volume that is hot
                as_double("pb_pump_coef"),                                          // [kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle
                as_double("tes_tank_cp") * 1000,                                    // convert to J/kgK
                as_double("tes_tank_dens"),                                         // Tank Wall density
                as_double("tes_tank_thick"),                                        // Tank wall thickness
                nstep,                                                              // Number subtimesteps
                as_vector_double("tes_NT_piston_loss_poly"),                        // Leakage polynomial (%)
                as_double("V_tes_des"),                                             // [m/s] Design-point velocity for sizing the diameters of the TES piping
                as_boolean("calc_design_pipe_vals"),                                // [-] Should the HTF state be calculated at design conditions
                as_double("tes_pump_coef"),                                         // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
                as_double("eta_pump"),                                              // [-] Pump efficiency, for newer pumping calculations
                as_boolean("has_hot_tank_bypass"),                                  // [-] True if the bypass valve causes the field htf to bypass just the hot tank and enter the cold tank before flowing back to the field.
                as_double("T_tank_hot_inlet_min"),                                  // [C] Minimum field htf temperature that may enter the hot tank
                false,                                                              // [-] True if the TES piping losses should be calculated using the TES pipe lengths and minor loss coeffs, false if using the pumping loss parameters
                false,                                                              // [-] True if the TES diameters and wall thicknesses parameters should be used instead of calculating them
                as_matrix("k_tes_loss_coeffs"),                                     // [-] Combined minor loss coefficients of the fittings and valves in the collection (including bypass) and generation loops in the TES 
                tes_diams,                                                          // [m] Imported inner diameters for the TES piping as read from the modified output files
                tes_wallthicks,                                                     // [m] Imported wall thicknesses for the TES piping as read from the modified output files
                tes_lengths,                                                        // [m] Imported lengths for the TES piping as read from the modified output files
                as_double("HDR_rough"),                                             // [m] Pipe absolute roughness
                as_double("DP_SGS")                                                 // [bar] Pressure drop on the TES discharge side (e.g., within the steam generator)
            );

            storage_pointer = &storage_NT;
        }
        // Particle Thermocline
        else if (tes_type == 2)
        {
            storage_particle = C_csp_particlecline_tes(
                as_integer("Fluid"),                                                // [-] field fluid identifier
                as_matrix("field_fl_props"),                                        // [-] field fluid properties
                as_double("T_loop_in_des"),                                         // [C] Cold design temperature
                as_double("T_loop_out"),                                            // [C] hot design temperature
                as_double("T_tank_hot_ini"),                                        // [C] Initial temperature in hot storage tank
                as_double("T_tank_cold_ini"),                                       // [C] Initial temperature in cold storage cold
                as_double("init_hot_htf_percent"),                                  // [%] Initial fraction of available volume that is hot
                as_integer("n_xsteps"),                                             // number spatial sub steps
                as_integer("n_tsteps"),                                             // number subtimesteps
                as_double("tes_pump_coef")                                          // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop 
                );


            vector<double> T_grad_ini = as_vector_double("T_grad_ini");
            if (T_grad_ini.size() > 1 && T_grad_ini[0] != -274)
            {
                if (T_grad_ini.size() != as_integer("n_xsteps") + 1)
                {
                    throw exec_error("csp_subcomponent", "Initial temperature gradient should be length n_xsteps + 1");
                }
                else
                {
                    storage_particle.set_T_grad_init(T_grad_ini);
                }
            }

            storage_pointer = &storage_particle;

        }
        else
        {
            throw exec_error("csp_subcomponent", "tes_type must be 0-2");
        }

        

        // Initialization   -> this is necessary to fully instantiate the TES
        C_csp_tes::S_csp_tes_init_inputs init_inputs;
        init_inputs.T_to_cr_at_des = C_to_K(as_double("T_loop_in_des"));        // [K]
        init_inputs.T_from_cr_at_des = C_to_K(as_double("T_loop_out"));         // [K]
        init_inputs.P_to_cr_at_des = 19.64;                                     // [bar]
        storage_pointer->init(init_inputs);

        // Get inputs
        double t_step = as_double("t_step");
        std::vector<double> T_amb = as_vector_double("T_amb");
        std::vector<double> mdot_src = as_vector_double("mdot_src");
        std::vector<double> mdot_sink = as_vector_double("mdot_sink");
        std::vector<double> hot_tank_bypassed = as_vector_double("hot_tank_bypassed");
        std::vector<double> T_src_out = as_vector_double("T_src_out");
        std::vector<double> T_sink_out = as_vector_double("T_sink_out");

        // Verify inputs
        size_t n_steps = T_amb.size();
        if (  mdot_src.size() != n_steps ||
              mdot_sink.size() != n_steps ||
              T_src_out.size() != n_steps ||
              T_sink_out.size() != n_steps) {
            throw exec_error("csp_subcomponent", "Input arrays not equal in size.");
        }

        // Get Design Point Outputs
        double V_tes_htf_avail_calc /*m3*/, V_tes_htf_total_calc /*m3*/,
            h_tank_calc /*m*/, d_tank_calc /*m*/, q_dot_loss_tes_des_calc /*MWt*/, dens_store_htf_at_T_ave_calc /*kg/m3*/,
            Q_tes_des_calc /*MWt-hr*/;

        if (tes_type == 0)
        {
            storage_two_tank.get_design_parameters(V_tes_htf_avail_calc, V_tes_htf_total_calc,
                h_tank_calc, d_tank_calc, q_dot_loss_tes_des_calc, dens_store_htf_at_T_ave_calc, Q_tes_des_calc);
        }
        else if (tes_type == 1)
        {
            storage_NT.get_design_parameters(V_tes_htf_avail_calc, V_tes_htf_total_calc,
                h_tank_calc, d_tank_calc, q_dot_loss_tes_des_calc, dens_store_htf_at_T_ave_calc, Q_tes_des_calc);
        }

        // Allocate outputs
        double* T_src_in = allocate("T_src_in", n_steps);
        double* T_sink_in = allocate("T_sink_in", n_steps);
        double* T_tank_cold = allocate("T_tank_cold", n_steps);
        double* T_tank_hot = allocate("T_tank_hot", n_steps);
        double* hot_tank_vol_frac = allocate("hot_tank_vol_frac", n_steps);
        double* W_dot_elec_in_tot = allocate("W_dot_elec_in_tot", n_steps);

        vector<double> piston_loc_vec;
        vector<double> piston_frac_vec;
        vector<double> T_hot_calc_vec;
        vector<double> T_cold_calc_vec;
        vector<double> tes_error_vec;
        vector<double> tes_error_percent_vec;
        vector<double> tes_error_hot_vec;
        vector<double> tes_error_cold_vec;
        vector<double> tes_error_leakage_vec;
        vector<double> tes_E_hot_vec;
        vector<double> tes_E_cold_vec;
        vector<double> tes_wall_error_vec;
        vector<double> tes_error_corrected_vec;
        util::matrix_t<ssc_number_t> tes_T_grad_mat;


        if (tes_type == 2)
        {
            tes_T_grad_mat.resize(n_steps, as_integer("n_xsteps") + 1);
        }

        // Simulate
        for (size_t i = 0; i < n_steps; i++) {
            double mdot_src_to_hot_tank = hot_tank_bypassed.at(i) ? 0. : mdot_src.at(i);
            double mdot_src_to_cold_tank = hot_tank_bypassed.at(i) ? mdot_src.at(i) : 0.;
            double T_src_in_K, T_sink_in_K;
            C_csp_tes::S_csp_tes_outputs tes_outputs;
            int result = storage_pointer->solve_tes_off_design(
                t_step,                         /*s*/
                C_to_K(T_amb.at(i)),            /*K*/
                mdot_src_to_hot_tank,           /*kg/s*/
                mdot_sink.at(i),                /*kg/s*/
                mdot_src_to_cold_tank,          /*kg/s*/
                C_to_K(T_src_out.at(i)),        /*K*/
                C_to_K(T_sink_out.at(i)),       /*K*/
                // Outputs:
                T_sink_in_K,                    /*K*/
                T_src_in_K,                     /*K*/
                tes_outputs);
            storage_pointer->converged();

            // Set outputs
            T_src_in[i] = K_to_C(T_src_in_K);
            T_sink_in[i] = K_to_C(T_sink_in_K);
            T_tank_cold[i] = K_to_C(storage_pointer->get_cold_temp());
            T_tank_hot[i] = K_to_C(storage_pointer->get_hot_temp());
            //assign("tes_diameter", d_tank_calc);
            //assign("tes_radius", d_tank_calc / 2.0);

            hot_tank_vol_frac[i] = storage_pointer->get_hot_tank_vol_frac();
            W_dot_elec_in_tot[i] = tes_outputs.m_W_dot_elec_in_tot;

            // Add NT specific outputs
            if (tes_type == 1)
            {
                double piston_location, piston_fraction;
                storage_NT.calc_piston_location(piston_location, piston_fraction);

                piston_loc_vec.push_back(piston_location);
                piston_frac_vec.push_back(piston_fraction);

                double tes_error = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_ERROR);
                double tes_error_percent = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_ERROR_PERCENT);
                double tes_error_hot = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_HOT_ERROR);
                double tes_error_cold = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_COLD_ERROR);
                double tes_error_leak = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_LEAK_ERROR);
                double tes_E_hot = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_E_HOT);
                double tes_E_cold = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_E_COLD);
                double tes_wall_error = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_WALL_ERROR);
                double tes_error_corrected = storage_NT.mc_reported_outputs.value(C_csp_NTHeatTrap_tes::E_ERROR_CORRECTED);

                tes_error_vec.push_back(tes_error);
                tes_error_percent_vec.push_back(tes_error_percent);
                tes_error_hot_vec.push_back(tes_error_hot);
                tes_error_cold_vec.push_back(tes_error_cold);
                tes_error_leakage_vec.push_back(tes_error_leak);
                tes_E_hot_vec.push_back(tes_E_hot);
                tes_E_cold_vec.push_back(tes_E_cold);
                tes_wall_error_vec.push_back(tes_wall_error);
                tes_error_corrected_vec.push_back(tes_error_corrected);
            }

            // Add Particle specific outputs
            {
                std::vector<double> T_prev_vec = storage_particle.get_T_prev_vec();
                for (int j = 0; j < T_prev_vec.size(); j++)
                {
                    tes_T_grad_mat.at(i, j) = T_prev_vec[j] - 273.15;   // [C] Convert from K
                }
            }

            // Simulate Analytically
            if (tes_type == 1)
            {
                double mdot_hot_net = mdot_src.at(i) - mdot_sink.at(i);
                double mdot_cold_net = -1.0 * mdot_hot_net;

                double mdot_net = mdot_src.at(i) - mdot_sink.at(i);

                double T_hot = T_src_out.at(i);
                double mdot_hot_in = mdot_src.at(i) - mdot_sink.at(i);

                double T_cold = T_sink_out.at(i);
                double mdot_cold_in = -1.0 * mdot_hot_in;


                // Hot Tank
                double T_hot_wall_in = 0;
                if (mdot_hot_net > 0)
                    T_hot_wall_in = T_sink_out.at(i);
                else
                    T_hot_wall_in = T_src_out.at(i);

                double V_hot_initial = V_tes_htf_total_calc * as_double("init_hot_htf_percent") * 0.01;

                //mdot_hot_net = mdot_hot_net < 0 ? 0 : mdot_hot_net;
                double T_hot_old_no_leak = simulate_TES(i, d_tank_calc / 2.0, mdot_hot_net, T_src_out.at(i), T_hot_wall_in, V_hot_initial, T_src_out.at(i));
                double T_hot_calc = simulate_TES_wLeakage(i, d_tank_calc / 2.0, mdot_net, T_hot, T_cold, V_hot_initial, T_hot);
                T_hot_calc_vec.push_back(T_hot_calc);


                // Cold Tank
                double T_cold_wall_in = 0;
                if (mdot_cold_net > 0)
                    T_cold_wall_in = T_src_out.at(i);
                else
                    T_cold_wall_in = T_sink_out.at(i);

                double V_cold_initial = V_tes_htf_total_calc * (1.0 - (as_double("init_hot_htf_percent") * 0.01));

                //mdot_cold_net = mdot_cold_net < 0 ? 0 : mdot_cold_net;
                double T_cold_old_no_leak = simulate_TES(i, d_tank_calc / 2.0, mdot_cold_net, T_sink_out.at(i), T_cold_wall_in, V_cold_initial, T_sink_out.at(i));
                double T_cold_calc = simulate_TES_wLeakage(i, d_tank_calc / 2.0, -1.0 * mdot_net, T_cold, T_hot, V_cold_initial, T_cold);
                T_cold_calc_vec.push_back(T_cold_calc);
            }
        }

        if (tes_type == 1)
        {
            set_vector("piston_loc", piston_loc_vec);
            set_vector("piston_frac", piston_frac_vec);
            set_vector("T_hot_calc", T_hot_calc_vec);
            set_vector("T_cold_calc", T_cold_calc_vec);
            set_vector("tes_error", tes_error_vec);
            set_vector("tes_error_percent", tes_error_percent_vec);
            set_vector("tes_hot_error", tes_error_hot_vec);
            set_vector("tes_cold_error", tes_error_cold_vec);
            set_vector("tes_leak_error", tes_error_leakage_vec);
            set_vector("tes_E_hot", tes_E_hot_vec);
            set_vector("tes_E_cold", tes_E_cold_vec);
            set_vector("tes_wall_error", tes_wall_error_vec);
            set_vector("tes_error_corrected", tes_error_corrected_vec);
        }

        if (tes_type == 2)
        {
            assign("T_grad_final", tes_T_grad_mat);
        }

    }

    double simulate_TES_charge(int index, double radius_inner)
    {
        double timestep = as_double("t_step");

        // Fluid Properties
        HTFProperties fluid_props;
        fluid_props.SetFluid(as_integer("Fluid"));

        // Energy Exchange
        double T_fluid_in = as_vector_double("T_src_out")[index];
        double T_cold_side = as_vector_double("T_sink_out")[index];
        double mdot_fluid_in = as_vector_double("mdot_src")[index];
        double T_wall_in = T_cold_side;

        // Tank Dimensions
        double wall_thickness = as_double("tes_tank_thick");            // m
        double wall_dens = as_double("tes_tank_dens");                  // kg/m3
        double wall_cp = as_double("tes_tank_cp") * 1e3;                // J/kg K

        // Calculate Total Fluid Mass Joining System
        double mass_fluid_in = mdot_fluid_in * timestep;
        double rho_fluid_in = fluid_props.dens(T_fluid_in + 273.15, 1.0);           // kg/m3
        double fluid_cp_in = fluid_props.Cp(T_fluid_in + 273.15) * 1e3;                      // J/kg K
        double V_fluid_in = mass_fluid_in / rho_fluid_in;                           // m3
        double L_fluid_in = V_fluid_in / (CSP::pi * std::pow(radius_inner, 2.0));   // m

        // Calculate Total Wall Mass Joining System
        double V_wall_in = L_fluid_in * CSP::pi * (std::pow(radius_inner + wall_thickness, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_in = wall_dens * V_wall_in;

        // Calculate Energy Balance
        double mass_total_initial = 0;  // Tank is fully discharged (no hot storage)
        double mass_total_final = mass_fluid_in + mass_wall_in;
        double cp_weighted_final = ((mass_fluid_in * fluid_cp_in) + (mass_wall_in * wall_cp)) / mass_total_final;   // J/kg K

        double energy_wall_in = mass_wall_in * wall_cp * T_wall_in;  // J
        double energy_fluid_in = mass_fluid_in * fluid_cp_in * T_fluid_in;   // J

        double net_energy_in = energy_wall_in + energy_fluid_in;

        double T_total_final = net_energy_in / (mass_total_final * cp_weighted_final);

        
        return T_total_final;
    }

    double simulate_TES_discharge(int index, double radius_inner, double T_hot_eq /*C*/)
    {
        double timestep = as_double("t_step");

        // Fluid Properties
        HTFProperties fluid_props;
        fluid_props.SetFluid(as_integer("Fluid"));

        // Energy Exchange
        double T_fluid_in = as_vector_double("T_sink_out")[index];  // C (cold inlet)
        double mdot_fluid_in = as_vector_double("mdot_sink")[index];    // C (cold mdot in)
        double T_wall_in = T_hot_eq;    // C (hot equilibrium temp)

        // Tank Dimensions
        double wall_thickness = as_double("tes_tank_thick");            // m
        double wall_dens = as_double("tes_tank_dens");                  // kg/m3
        double wall_cp = as_double("tes_tank_cp") * 1e3;                // J/kg K

        // Calculate Total Fluid Mass Joining System
        double mass_fluid_in = mdot_fluid_in * timestep;
        double rho_fluid_in = fluid_props.dens(T_fluid_in + 273.15, 1.0);           // kg/m3
        double fluid_cp_in = fluid_props.Cp(T_fluid_in + 273.15) * 1e3;                      // J/kg K
        double V_fluid_in = mass_fluid_in / rho_fluid_in;                           // m3
        double L_fluid_in = V_fluid_in / (CSP::pi * std::pow(radius_inner, 2.0));   // m

        // Calculate Total Wall Mass Joining System
        double V_wall_in = L_fluid_in * CSP::pi * (std::pow(radius_inner + wall_thickness, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_in = wall_dens * V_wall_in;

        // Calculate Energy Balance
        double mass_total_initial = 0;  // Tank is fully discharged (no hot storage)
        double mass_total_final = mass_fluid_in + mass_wall_in;
        double cp_weighted_final = ((mass_fluid_in * fluid_cp_in) + (mass_wall_in * wall_cp)) / mass_total_final;   // J/kg K

        double energy_wall_in = mass_wall_in * wall_cp * T_wall_in;  // J
        double energy_fluid_in = mass_fluid_in * fluid_cp_in * T_fluid_in;   // J

        double net_energy_in = energy_wall_in + energy_fluid_in;

        double T_total_final = net_energy_in / (mass_total_final * cp_weighted_final);


        return T_total_final;
    }

    double simulate_TES(int index, double radius_inner, double mdot_fluid_in, double T_fluid_in, double T_wall_in,
                        double V_fluid_initial, double T_fluid_initial)
    {
        double timestep = as_double("t_step");

        // Fluid Properties
        HTFProperties fluid_props;
        fluid_props.SetFluid(as_integer("Fluid"));

        // Energy Exchange
        //double T_fluid_in = as_vector_double("T_src_out")[index];
        //double T_cold_side = as_vector_double("T_sink_out")[index];
        //double mdot_fluid_in = as_vector_double("mdot_src")[index];
        //double T_wall_in = T_cold_side;

        // Tank Dimensions
        double thickness_wall = as_double("tes_tank_thick");            // m
        double rho_wall = as_double("tes_tank_dens");                  // kg/m3
        double cp_wall = as_double("tes_tank_cp") * 1e3;                // J/kg K

        // Calculate Total Fluid Mass Joining System
        double mass_fluid_in = mdot_fluid_in * timestep;
        double rho_fluid_in = fluid_props.dens(T_fluid_in + 273.15, 1.0);           // kg/m3
        double cp_fluid_in = fluid_props.Cp(T_fluid_in + 273.15) * 1e3;                      // J/kg K
        double V_fluid_in = mass_fluid_in / rho_fluid_in;                           // m3
        double L_fluid_in = V_fluid_in / (CSP::pi * std::pow(radius_inner, 2.0));   // m

        // Calculate Total Wall Mass Joining System
        double V_wall_in = L_fluid_in * CSP::pi * (std::pow(radius_inner + thickness_wall, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_in = rho_wall * V_wall_in;

        // Calculate Wall Mass
        double rho_fluid_initial = fluid_props.dens(T_fluid_initial + 273.15, 1.0); // kg/m3
        double mass_fluid_initial = V_fluid_initial * rho_fluid_initial;            // m3
        double L_fluid_initial = V_fluid_initial / (CSP::pi * std::pow(radius_inner, 2.0));   // m
        double V_wall_initial = L_fluid_initial * CSP::pi * (std::pow(radius_inner + thickness_wall, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_initial = rho_wall * V_wall_initial;
        double mass_total_initial = mass_fluid_initial + mass_wall_initial;

        // Calculate Initial Total Energy
        double cp_fluid_initial = fluid_props.Cp(T_fluid_initial + 273.15) * 1e3;   // J/kg K
        double energy_fluid_initial = mass_fluid_initial * cp_fluid_initial * T_fluid_initial;
        double energy_wall_initial = mass_wall_initial * cp_wall * T_fluid_initial;
        double energy_total_initial = energy_fluid_initial + energy_wall_initial;   // J

        // Calculate Energy Balance
        double mass_total_final = mass_fluid_in + mass_wall_in + mass_total_initial;

        double cp_weighted_final = ((mass_fluid_in * cp_fluid_in) + (mass_wall_in * cp_wall) +
                (mass_fluid_initial * cp_fluid_initial) + (mass_wall_initial * cp_wall)) / mass_total_final;   // J/kg K

        double energy_wall_in = mass_wall_in * cp_wall * T_wall_in;  // J
        double energy_fluid_in = mass_fluid_in * cp_fluid_in * T_fluid_in;   // J

        double net_energy = energy_wall_in + energy_fluid_in + energy_total_initial;

        double T_total_final = net_energy / (mass_total_final * cp_weighted_final);


        return T_total_final;
    }


    /// <param name="mdot_fluid">Positive is incoming, negative is outgoing</param>
    double simulate_TES_wLeakage(int index, double radius_inner,
        double mdot_fluid_net, double T_fluid_in, double T_wall_in,
        double V_fluid_initial, double T_fluid_initial)
    {
        double timestep = as_double("t_step");

        // Fluid Properties
        int store_fluid = as_integer("store_fluid");
        util::matrix_t<double> store_fl_props = as_matrix("store_fl_props");

        HTFProperties fluid_props;
        // Declare instance of fluid class for STORAGE fluid.
        // Set fluid number and copy over fluid matrix if it makes sense.
        if (store_fluid != HTFProperties::User_defined && store_fluid < HTFProperties::End_Library_Fluids)
        {
            if (!fluid_props.SetFluid(store_fluid))
            {
                throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
            }
        }
        else if (store_fluid == HTFProperties::User_defined)
        {
            int n_rows = (int)store_fl_props.nrows();
            int n_cols = (int)store_fl_props.ncols();
            if (n_rows > 2 && n_cols == 7)
            {
                if (!fluid_props.SetUserDefinedFluid(store_fl_props))
                {
                    std::string error_msg = util::format(fluid_props.UserFluidErrMessage(), n_rows, n_cols);
                    throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
                }
            }
            else
            {
                std::string error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
            }
        }
        else
        {
            throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
        }

        // Tank Dimensions
        double thickness_wall = as_double("tes_tank_thick");            // m
        double rho_wall = as_double("tes_tank_dens");                  // kg/m3
        double cp_wall = as_double("tes_tank_cp") * 1e3;                // J/kg K

        // Define Fluid Mass Flows
        double mdot_fluid_out = mdot_fluid_net > 0 ? 0 : -1 * mdot_fluid_net;
        double mdot_fluid_in = mdot_fluid_net > 0 ? mdot_fluid_net : 0;

        // Calculate Leakage
        double mdot_fluid_leak_in;
        double mdot_fluid_leak_out;
        double T_fluid_leak_in = T_wall_in;
        double T_fluid_leak_out = T_fluid_initial;
        {
            std::vector<double> leak_poly = as_vector_double("tes_NT_piston_loss_poly");
            double leak_frac_in = 0;
            for (int i = 0; i < leak_poly.size(); i++)
            {
                leak_frac_in += leak_poly[i] * std::pow(mdot_fluid_out, i) * 0.01;
            }
            mdot_fluid_leak_in = leak_frac_in * mdot_fluid_out;

            double leak_frac_out = 0;
            for (int i = 0; i < leak_poly.size(); i++)
            {
                leak_frac_out += leak_poly[i] * std::pow(mdot_fluid_in, i) * 0.01;
            }
            mdot_fluid_leak_out = leak_frac_out * mdot_fluid_in;
        }

        // Calculate Inlet Fluid Mass Joining System
        double mass_fluid_in = mdot_fluid_in * timestep;                                        // kg
        double rho_fluid_in = fluid_props.dens(T_fluid_in + 273.15, 1.0);                       // kg/m3
        double cp_fluid_in = fluid_props.Cp(T_fluid_in + 273.15) * 1e3;                         // J/kg K
        double V_fluid_in = mass_fluid_in / rho_fluid_in;                                       // m3
        double L_fluid_in = V_fluid_in / (CSP::pi * std::pow(radius_inner, 2.0));               // m

        // Calculate Outlet Fluid Mass Leaving System
        double mass_fluid_out = mdot_fluid_out * timestep;                                      // kg
        double rho_fluid_out = fluid_props.dens(T_fluid_initial + 273.15, 1.0);                 // kg/m3
        double cp_fluid_out = fluid_props.Cp(T_fluid_initial + 273.15) * 1e3;                   // J/kg K
        double V_fluid_out = mass_fluid_out / rho_fluid_out;                                    // m3
        double L_fluid_out = V_fluid_out / (CSP::pi * std::pow(radius_inner, 2.0));             // m

        // Calculate Fluid Leak Joining System      
        double mass_fluid_leak_in = mdot_fluid_leak_in * timestep;                              // kg
        double rho_fluid_leak_in = fluid_props.dens(T_fluid_leak_in + 273.15, 1.0);             // kg/m3
        double cp_fluid_leak_in = fluid_props.Cp(T_fluid_leak_in + 273.15) * 1e3;               // J/kg K
        double V_fluid_leak_in = mass_fluid_leak_in / rho_fluid_leak_in;                        // m3
        double L_fluid_leak_in = V_fluid_leak_in / (CSP::pi * std::pow(radius_inner, 2.0));     // m  

        // Calculate Fluid Leak Leaving System
        double mass_fluid_leak_out = mdot_fluid_leak_out * timestep;                            // kg
        double rho_fluid_leak_out = fluid_props.dens(T_fluid_leak_out + 273.15, 1.0);           // kg/m3
        double cp_fluid_leak_out = fluid_props.Cp(T_fluid_leak_out + 273.15) * 1e3;             // J/kg K
        double V_fluid_leak_out = mass_fluid_leak_out / rho_fluid_leak_out;                     // m3
        double L_fluid_leak_out = V_fluid_leak_out / (CSP::pi * std::pow(radius_inner, 2.0));   // m  

        // Calculate Total Length Change
        double L_net_delta = L_fluid_in + L_fluid_leak_in - L_fluid_out - L_fluid_leak_out;           // m

        // Calculate Total Wall Mass Joining System
        double V_wall_in = L_net_delta * CSP::pi * (std::pow(radius_inner + thickness_wall, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_delta = rho_wall * V_wall_in;
        double mass_wall_in = mass_wall_delta > 0 ? mass_wall_delta : 0;
        double mass_wall_out = mass_wall_delta > 0 ? 0 : -1 * mass_wall_delta;

        // Calculate Initial Wall Mass
        double rho_fluid_initial = fluid_props.dens(T_fluid_initial + 273.15, 1.0); // kg/m3
        double mass_fluid_initial = V_fluid_initial * rho_fluid_initial;            // m3
        double L_fluid_initial = V_fluid_initial / (CSP::pi * std::pow(radius_inner, 2.0));   // m
        double V_wall_initial = L_fluid_initial * CSP::pi * (std::pow(radius_inner + thickness_wall, 2.0) - std::pow(radius_inner, 2.0));
        double mass_wall_initial = rho_wall * V_wall_initial;

        // Calculate Initial Total Energy
        double mass_total_initial = mass_fluid_initial + mass_wall_initial;
        double cp_fluid_initial = fluid_props.Cp(T_fluid_initial + 273.15) * 1e3;   // J/kg K
        double energy_fluid_initial = mass_fluid_initial * cp_fluid_initial * T_fluid_initial;
        double energy_wall_initial = mass_wall_initial * cp_wall * T_fluid_initial;
        double energy_total_initial = energy_fluid_initial + energy_wall_initial;   // J

        // Calculate Energy Balance
        double mass_total_final = mass_fluid_in + mass_fluid_leak_in + mass_wall_in
                                  - mass_fluid_out - mass_fluid_leak_out - mass_wall_out
                                  + mass_total_initial;

        double cp_weighted_final = ((mass_fluid_in * cp_fluid_in)
                                    + (mass_fluid_leak_in * cp_fluid_leak_in)
                                    + (mass_wall_in * cp_wall)
                                    - (mass_fluid_out * cp_fluid_out)
                                    - (mass_fluid_leak_out * cp_fluid_leak_out)
                                    - (mass_wall_out * cp_wall)
                                    + (mass_fluid_initial * cp_fluid_initial)
                                    + (mass_wall_initial * cp_wall)) / mass_total_final;   // J/kg K

        double energy_wall_in = mass_wall_in * cp_wall * T_wall_in;  // J
        double energy_fluid_in = mass_fluid_in * cp_fluid_in * T_fluid_in;   // J
        double energy_fluid_leak_in = mass_fluid_leak_in * cp_fluid_leak_in * T_fluid_leak_in;

        double energy_wall_out = mass_wall_out * cp_wall * T_fluid_initial;
        double energy_fluid_out = mass_fluid_out * cp_fluid_out * T_fluid_initial;
        double energy_fluid_leak_out = mass_fluid_leak_out * cp_fluid_leak_out * T_fluid_leak_out;

        double energy_delta = energy_fluid_in + energy_fluid_leak_in + energy_wall_in
                            - energy_fluid_out - energy_fluid_leak_out - energy_wall_out;

        double energy_final = energy_total_initial + energy_delta;

        double T_total_final = energy_final / (mass_total_final * cp_weighted_final);


        return T_total_final;
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

double C_to_K(double T) {
    return T + 273.15;
}

double K_to_C(double T) {
    return T - 273.15;
}

DEFINE_MODULE_ENTRY(csp_subcomponent, "CSP subcomponents", 1)
