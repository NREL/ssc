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

#include "ptes_solver_design_point.h"

#include <algorithm>
using std::string;

static var_info _cm_vtab_ptes_design_point[] = {

    // ----------------------------------------- Input

    // Simulation options
    //   VARTYPE    DATATYPE        NAME                                LABEL                                                       UNITS           META            GROUP               REQUIRED_IF         CONSTRAINTS     UI_HINTS*/

    { SSC_INPUT,    SSC_NUMBER,     "hx_eff",                           "hx effectiveness",                                         "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "eta",                              "polytropic efficiency of compressors and expanders",       "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "eta_pump",                         "polytropic efficiency of air pump",                        "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "ploss_working",                    "Fractional pressure loss in each heat exchanger",          "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "ploss_air",                        "Fractional pressure loss (air)",                           "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "ploss_liquid",                     "Fractional pressure loss (liquid)",                        "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "motor_eff",                        "Motor Efficiency",                                         "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "gen_eff",                          "Generator Efficiency",                                     "",             "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "T0",                               "Ambient Temperature",                                      "K",            "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "P0",                               "Ambient Pressure",                                         "Pa",           "",             "",                     "*",              "",              ""},
                                                                                                                                                                                                              
    { SSC_INPUT,    SSC_NUMBER,     "P1",                               "Lowest Pressure in cycle",                                 "Pa",           "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "T_compressor_inlet",               "Charging compressor inlet temperature",                    "K",            "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "T_compressor_outlet",              "Charging compressor outlet temperature",                   "K",            "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "power_output",                     "Power Output",                                             "W",            "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "charge_time_hr",                   "charging time",                                            "hr",           "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "discharge_time_hr",                "discharge time",                                           "hr",           "",             "",                     "*",              "",              ""},
    { SSC_INPUT,    SSC_NUMBER,     "alpha",                            "Ratio of mdot cp     AIR/WF",                              "",             "",             "",                     "*",              "",              ""},
                                                                                                                                                                                                              
    { SSC_INPUT,    SSC_STRING,     "working_fluid_type",               "Working Fluid Fluid",                                      "",             "",             "",                     "",              "",              ""},
    { SSC_INPUT,    SSC_STRING,     "hot_fluid_type",                   "Hot Resevoir Fluid",                                       "",             "",             "",                     "",              "",              ""},
    { SSC_INPUT,    SSC_STRING,     "cold_fluid_type",                  "Cold Resevoir Fluid",                                      "",             "",             "",                     "",              "",              ""},

    { SSC_INPUT,    SSC_STRING,     "hot_cp",                           "Hot Resevoir Specific Heat",                               "J/kg K",       "",             "",                     "",              "",              ""},
    { SSC_INPUT,    SSC_STRING,     "hot_rho",                          "Hot Resevoir Density",                                     "kg/m3",        "",             "",                     "",              "",              ""},
    { SSC_INPUT,    SSC_STRING,     "cold_cp",                          "Cold Resevoir Specific Heat",                              "J/kg K",       "",             "",                     "",              "",              ""},
    { SSC_INPUT,    SSC_STRING,     "cold_rho",                         "Cold Resevoir Density",                                    "kg/m3",        "",             "",                     "",              "",              ""},



    // ----------------------------------------- Output

    // Results
    { SSC_OUTPUT, SSC_NUMBER,       "hp_COP",                           "Heat Pump COP",                                            "",             "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "cycle_eff",                        "Cycle Efficiency",                                         "",             "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "Th_hot",                           "Hot Storage Hot Temp",                                     "C",            "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "Tc_hot",                           "Cold Storage Hot Temp",                                    "C",            "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "Tc_cold",                          "Cold Storage Cold Temp",                                   "C",            "",             "SAM",                     "",               "",              ""},
    
    { SSC_OUTPUT, SSC_NUMBER,       "hp_parasitic_fraction",            "Heat Pump Parasitics Fraction",                            "",             "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "hp_hot_pump_power",                "Heat Pump Hot HX Pump Power",                              "kW/kg/s",      "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "hp_cold_pump_power",               "Heat Pump Cold HX Pump Power",                             "kW/kg/s",      "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "pc_parasitic_fraction",            "Power Cycle Parasitics Fraction",                          "",            "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "pc_hot_pump_power",                "Power Cycle Hot HX Pump Power",                            "kW/kg/s",      "",             "SAM",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "pc_cold_pump_power",               "Power Cycle Cold HX Pump Power",                           "kW/kg/s",      "",             "SAM",                     "",               "",              ""},

     // Plots
    { SSC_OUTPUT, SSC_NUMBER,       "N_pts_charge",                     "Number data points on plot",                               "",             "",             "",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,        "temp_series_charge",               "Temperature Values",                                       "C",            "",             "",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,        "s_series_charge",                  "Entropy Values",                                           "kJ/kg K",      "",             "",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_NUMBER,       "N_pts_discharge",                  "Number data points on plot",                               "",             "",             "",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,        "temp_series_discharge",            "Temperature Values",                                       "C",            "",             "",                     "",               "",              ""},
    { SSC_OUTPUT, SSC_ARRAY,        "s_series_discharge",               "Entropy Values",                                           "kJ/kg K",      "",             "",                     "",               "",              ""},

    var_info_invalid };

class cm_ptes_design_point : public compute_module
{
public:

    cm_ptes_design_point()
    {
        add_var_info(_cm_vtab_ptes_design_point);
    }

    void exec() override
    {
        // Collect Inputs
        PTESSystemParam params;
        
        try
        {
            params.hx_eff = compute_module::as_double("hx_eff");
            params.eta = compute_module::as_double("eta");
            params.eta_pump = compute_module::as_double("eta_pump");
            params.ploss_working = compute_module::as_double("ploss_working");
            params.ploss_air = compute_module::as_double("ploss_air");
            params.ploss_liquid = compute_module::as_double("ploss_liquid");

            params.motor_eff = compute_module::as_double("motor_eff");
            params.gen_eff = compute_module::as_double("gen_eff");

            params.T0 = compute_module::as_double("T0");
            params.P0 = compute_module::as_double("P0");
            params.P1 = compute_module::as_double("P1");
            params.T_compressor_inlet = compute_module::as_double("T_compressor_inlet");
            params.T_compressor_outlet = compute_module::as_double("T_compressor_outlet");
            params.power_output = compute_module::as_double("power_output");
            params.charge_time_hr = compute_module::as_double("charge_time_hr");
            params.discharge_time_hr = compute_module::as_double("discharge_time_hr");
            params.alpha = compute_module::as_double("alpha");
        }
        catch (std::exception& e)
        {
            throw e;
        }

        // Collect Fluid Inputs
        FluidMaterialProp working_fluid(FluidType::kAir); // Placeholder Working Fluid
        FluidMaterialProp hot_fluid(FluidType::kAir);
        FluidMaterialProp cold_fluid(FluidType::kAir);
        try
        {
            // Assign Working Fluid
            string wf_string = compute_module::as_string("working_fluid_type");
            bool wf_flag;
            FluidType wf_type = PTESDesignPoint::GetFluidTypeFromString(wf_string, wf_flag);
            working_fluid = FluidMaterialProp(wf_type);

            // Check if HF and CF have Types or are custom

            // Hot Fluid
            bool hf_flag = compute_module::is_assigned("hot_fluid_type");
            if (hf_flag == false)
            {
                double hf_cp = compute_module::as_number("hot_cp");
                double hf_rho = compute_module::as_number("hot_rho");
                hot_fluid = FluidMaterialProp(hf_cp, hf_rho, params.T0, params.P0);
            }
            else
            {
                string hf_string = compute_module::as_string("hot_fluid_type");
                FluidType hf_type = PTESDesignPoint::GetFluidTypeFromString(hf_string, hf_flag);
                if (hf_flag == false)
                {
                    // Error
                    throw new std::exception("Error Reading Hot Fluid Type");
                }
                hot_fluid = FluidMaterialProp(hf_type);
            }

            // Cold Fluid
            bool cf_flag = compute_module::is_assigned("cold_fluid_type");
            if (cf_flag == false)
            {
                double cf_cp = compute_module::as_number("cold_cp");
                double cf_rho = compute_module::as_number("cold_rho");
                cold_fluid = FluidMaterialProp(cf_cp, cf_rho, params.T0, params.P0);
            }
            else
            {
                string cf_string = compute_module::as_string("cold_fluid_type");
                FluidType cf_type = PTESDesignPoint::GetFluidTypeFromString(cf_string, cf_flag);
                if (cf_flag == false)
                    throw new std::exception("Error Reading Cold Fluid Type");
                cold_fluid = FluidMaterialProp(cf_type);
            }

        }
        catch (std::exception e)
        {
            throw e;
        }

        // Build and Run System
        PTESDesignPoint ptes(params, working_fluid, hot_fluid, cold_fluid);
        ptes.Charge();
        ptes.Discharge();
        ptes.Performance();

        // Collect TS Data
        vector<double> t_vec, s_vec;
        ptes.GetTSDataCharge(t_vec, s_vec);
        int out_size = t_vec.size();

        vector<double> t_vec_D, s_vec_D;
        ptes.GetTSDataDischarge(t_vec_D, s_vec_D);
        int out_size_D = t_vec_D.size();

        // Send Output
        try
        {
            // Output Ts data
            ssc_number_t* ptr_temp_series_charge = allocate("temp_series_charge", out_size);
            ssc_number_t* ptr_s_series_charge = allocate("s_series_charge", out_size);
            for (int i = 0; i < out_size; i++)
            {
                ptr_temp_series_charge[i] = (ssc_number_t)t_vec[i];
                ptr_s_series_charge[i] = (ssc_number_t)s_vec[i];
            }

            ssc_number_t* ptr_temp_series_discharge = allocate("temp_series_discharge", out_size_D);
            ssc_number_t* ptr_s_series_discharge = allocate("s_series_discharge", out_size_D);
            for (int i = 0; i < out_size_D; i++)
            {
                ptr_temp_series_discharge[i] = (ssc_number_t)t_vec_D[i];
                ptr_s_series_discharge[i] = (ssc_number_t)s_vec_D[i];
            }

            assign("N_pts_charge", (ssc_number_t)out_size);
            assign("N_pts_discharge", (ssc_number_t)out_size_D);
            assign("hp_COP", (ssc_number_t)ptes.hp_COP_);
            assign("cycle_eff", (ssc_number_t)ptes.cycle_eff_);
            assign("Th_hot", (ssc_number_t)ptes.Th_hot_);
            assign("Tc_hot", (ssc_number_t)ptes.Tc_hot_);
            assign("Tc_cold", (ssc_number_t)ptes.Tc_cold_);
            assign("hp_parasitic_fraction", (ssc_number_t)ptes.hp_parasitic_fraction_);
            assign("hp_hot_pump_power", (ssc_number_t)ptes.hp_hot_pump_power_);
            assign("hp_cold_pump_power", (ssc_number_t)ptes.hp_cold_pump_power_);
            assign("pc_parasitic_fraction", (ssc_number_t)ptes.pc_parasitic_fraction_);
            assign("pc_hot_pump_power", (ssc_number_t)ptes.pc_hot_pump_power_);
            assign("pc_cold_pump_power", (ssc_number_t)ptes.pc_cold_pump_power_);
        }
        catch (std::exception& e)
        {
            throw e;
        }

    }

};

DEFINE_MODULE_ENTRY(ptes_design_point, "PTES Design Point", 1);
