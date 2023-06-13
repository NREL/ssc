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


#include <string>
#include <iostream>
#include <fstream>
#include <vector>

#include <gtest/gtest.h>
#include <lib_ortools.h>
#include "ortools/linear_solver/linear_solver.h"
#include <lib_ptes_chp_dispatch.h>

using namespace operations_research;

char input_data_path[512];
int data1 = sprintf(input_data_path, "%s/test/input_cases/ortools/", std::getenv("SSCDIR"));

TEST(libORToolsTest, test)
{
	bool build_for_server = false;
    bool run_rolling_horizon_cases = false;

    std::string results_file = "heat_valued_results_no_off_design_heat.csv";
    std::string res_dir = "heat_valued_no_offdes_results_1p_gap/";
    std::string input_dir = input_data_path;

    // Problem set-up
    ptes_chp_dispatch ptes_chp;
    ptes_chp.design.init(100. /*cycle capacity*/, 0.47 /*cycle efficiency*/, 1.33 /*heat pump COP*/, 10. /*hour of tes*/); 
    PTES_CHP_Dispatch_Data* data = &ptes_chp.params;
    data->n_periods = 100;
    data->delta = 1.0;

    // PTES and heat off-taker configuration
    ptes_chp.config.is_charge_heat_reject = false;
    ptes_chp.config.is_discharge_heat_reject = true;
    ptes_chp.config.is_heat_demand_required = false;
    ptes_chp.config.is_heat_from_tes_allowed = false;
    ptes_chp.config.is_offtaker_tes = false;
    ptes_chp.config.is_heat_valued = true;

    // Setting model parameters
    data->setPrices(input_dir + "generic_low_carbon_duck_curve.csv", 60.0, 20.0);
    data->setHeatLoad(input_dir + "heat_load.csv");
    data->setDefaultAssumptions(ptes_chp.design);

    struct run_time{
        int opt_horizon;
        int roll_horizon;
        double time;
    };

    std::vector<run_time> times;
    run_time run;
    run.opt_horizon = data->n_periods;
    run.roll_horizon = 0;
    run.time = ptes_chp.optimize(input_dir + res_dir + results_file);
    times.push_back(run);

    ptes_chp.solver->SuppressOutput(); // Turning off solver output for roll solves

    if (run_rolling_horizon_cases) {
        int opt_horizon = 0;
        int roll_horizon = 0;
        for (int roll_d = 1; roll_d <= 7; roll_d++) {
            roll_horizon = roll_d * 24;
            for (int opt_d = 1; opt_d <= 7; opt_d++) {
                opt_horizon = opt_d * 24;
                if (opt_horizon >= roll_horizon) {
                    LOG(INFO) << "Now solving an optimization horizon of " << opt_horizon
                        << " with a rolling horizon of " << roll_horizon
                        << std::endl;
                    std::string res_file_name = root + res_dir
                        + std::to_string(opt_horizon) + "w_" + std::to_string(roll_horizon) + "r_" + results_file;
                    run.time = ptes_chp.rollingHorizonoptimize(opt_horizon, roll_horizon, res_file_name);
                    run.opt_horizon = opt_horizon;
                    run.roll_horizon = roll_horizon;
                    times.push_back(run);
                }
            }
        }
    }

    LOG(INFO) << std::setfill(' ') << std::left << std::setw(15) << "Opt. Horizon"
        << std::left << std::setw(15) << "Roll Horizon"
        << std::left << std::setw(15) << "Time (sec)"
        << std::endl;
    for (int i = 0; i < times.size(); i++) {
        LOG(INFO) << std::setfill(' ') << std::left << std::setw(15) << times[i].opt_horizon
            << std::left << std::setw(15) << times[i].roll_horizon
            << std::left << std::setw(15) << times[i].time
            << std::endl;
    }

    std::ofstream outputfile;
    outputfile.open(root + res_dir + "times.txt", std::ios_base::app);
    std::string var_header = "Opt. Horizon, Roll Horizon, Time (sec)";
    outputfile << var_header << std::endl;
    for (int i = 0; i < times.size(); i++) {
        outputfile << times[i].opt_horizon
            << ", " << times[i].roll_horizon
            << ", " << times[i].time
            << std::endl;
    }
    outputfile.close();

    // return EXIT_SUCCESS;
}