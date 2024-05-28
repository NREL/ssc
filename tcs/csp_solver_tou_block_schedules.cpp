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

#include "csp_solver_util.h"
#include "csp_solver_core.h"
#include <algorithm>

#include "lib_util.h"

C_timeseries_schedule_inputs::C_timeseries_schedule_inputs(const util::matrix_t<double>& weekdays, const util::matrix_t<double>& weekends,
    std::vector<double> tod_factors)
{
    input_type = BLOCK;

    // util::translate_schedule enforces tou periods between harcoded min and max value
    //   and does not report an error or message if this happens

    int tod[8760];

    if (!util::translate_schedule(tod, weekdays, weekends, 1, 9)) {
        std::string m_error_msg = "TOU schedules must have 12 rows and 24 columns";
        throw C_csp_exception( m_error_msg, "TOU block schedule init" );
    }

    mv_timeseries_schedule_data.resize(8760);

    for (size_t i = 0; i < 8760; i++)
    {
        mv_timeseries_schedule_data[i].tou_period = tod[i];
        mv_timeseries_schedule_data[i].value = tod_factors[tod[i] - 1];
    }

}

C_timeseries_schedule_inputs::C_timeseries_schedule_inputs(std::vector<double>& timeseries_values_in)
{
    input_type = TIMESERIES;

    size_t nrecs = timeseries_values_in.size();
    if (nrecs <= 0)
    {
        std::string m_error_msg = util::format("The timestep price multiplier array was empty.");
        throw(C_csp_exception(m_error_msg, "C_timeseries_schedule_inputs"));
    }

    mv_timeseries_schedule_data.resize(nrecs);

    for (size_t i = 0; i < nrecs; i++) {
        mv_timeseries_schedule_data[i].tou_period = 1;
        mv_timeseries_schedule_data[i].value = timeseries_values_in[i];
    }
}

C_timeseries_schedule_inputs::C_timeseries_schedule_inputs(double const_val)
{
    input_type = CONSTANT;

    mv_timeseries_schedule_data.resize(8760);

    for (size_t i = 0; i < 8760; i++) {
        mv_timeseries_schedule_data[i].tou_period = 1;
        mv_timeseries_schedule_data[i].value = const_val;
    }
}

void C_timeseries_schedule_inputs::get_timestep_data(double time_s, double& val, int& tou)
{
    size_t nrecs = mv_timeseries_schedule_data.size();

    if (nrecs <= 0)
    {
        std::string m_error_msg = util::format("The timestep price multiplier array was empty.");
        throw(C_csp_exception(m_error_msg, "TOU timestep call"));
    }

    size_t nrecs_per_hour = nrecs / 8760;
    int ndx = (int)((ceil(time_s / 3600.0 - 1.e-6) - 1) * nrecs_per_hour);

    bool m_isleapyear = false;
    if (ndx > (int)nrecs - 1 + (m_isleapyear ? 24 : 0) || ndx < 0)
    {
        std::string m_error_msg = util::format("The index input to the TOU schedule must be from 1 to %d. The input timestep index was %d.", (int)nrecs, ndx + 1);
        throw(C_csp_exception(m_error_msg, "TOU timestep call"));
    }

    val = mv_timeseries_schedule_data[ndx].value;
    tou = mv_timeseries_schedule_data[ndx].tou_period;
}

void C_csp_tou::init(bool dispatch_optimize)
{
    // Check that dispatch logic is reasonable
    if (!(dispatch_optimize || mc_dispatch_params.m_is_block_dispatch || mc_dispatch_params.m_is_arbitrage_policy || mc_dispatch_params.m_is_dispatch_targets))
    {
        throw(C_csp_exception("Must select a plant control strategy", "TOU initialization"));
    }

    if ((dispatch_optimize && mc_dispatch_params.m_is_block_dispatch) ||
        (dispatch_optimize && mc_dispatch_params.m_is_arbitrage_policy) ||
        (dispatch_optimize && mc_dispatch_params.m_is_dispatch_targets) ||
        (mc_dispatch_params.m_is_block_dispatch && mc_dispatch_params.m_is_arbitrage_policy) ||
        (mc_dispatch_params.m_is_block_dispatch && mc_dispatch_params.m_is_dispatch_targets) ||
        (mc_dispatch_params.m_is_arbitrage_policy && mc_dispatch_params.m_is_dispatch_targets))
    {
        throw(C_csp_exception("Multiple plant control strategies were selected. Please select one.", "TOU initialization"));
    }

    if (mc_dispatch_params.m_is_block_dispatch)
    {
        if (mc_dispatch_params.m_use_rule_1)
        {
            if (mc_dispatch_params.m_standby_off_buffer < 0.0)
            {
                throw(C_csp_exception("Block Dispatch Rule 1 was selected, but the time entered was invalid."
                    " Please select a time >= 0", "TOU initialization"));
            }
        }

        if (mc_dispatch_params.m_use_rule_2)
        {
            if (mc_dispatch_params.m_f_q_dot_pc_overwrite <= 0.0 ||
                mc_dispatch_params.m_q_dot_rec_des_mult <= 0.0)
            {
                throw(C_csp_exception("Block Dispatch Rule 2 was selected, but the parameters entered were invalid."
                    " Both values must be greater than 0", "TOU initialization"));
            }
        }
    }
}

void C_csp_tou::call(double time_s, C_csp_tou::S_csp_tou_outputs& tou_outputs)
{
    mc_offtaker_schedule.get_timestep_data(time_s, tou_outputs.m_f_turbine, tou_outputs.m_csp_op_tou);
    mc_elec_pricing_schedule.get_timestep_data(time_s, tou_outputs.m_price_mult, tou_outputs.m_pricing_tou);

    if (mc_dispatch_params.m_is_tod_pc_target_also_pc_max) {
        tou_outputs.m_wlim_dispatch = tou_outputs.m_f_turbine;
    }
    else {
        tou_outputs.m_wlim_dispatch = 9.e99;
    }
}
