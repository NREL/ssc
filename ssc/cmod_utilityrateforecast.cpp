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

#include "common.h"
#include "core.h"

#include "cmod_utilityrate5.h"
#include "cmod_utilityrateforecast.h"

static var_info vtab_utilityrateforecast[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                                  UNITS     META       GROUP           REQUIRED_IF     CONSTRAINTS UI_HINTS*/
    { SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in escalation and forecast", "years",  "",  "Lifetime",     "*",           "INTEGER,POSITIVE",              "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hr",                    "Time step in hours",                  "hr",      "",        "Controls",     "*",            "",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "last_idx",                 "Last index (lifetime)",               "",        "",        "StatePack",    "",             "",         ""  },

    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_load",          "Monthly load forecast",               "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_gen",           "Monthly generation forecast",         "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_peaks",         "Monthly peaks forecast",              "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "grid_power",               "Electricity to/from grid",            "",        "",        "Electricity Rates",   "",      "",         "" },

    { SSC_INOUT,        SSC_MATRIX,      "ur_net_metering_credits",  "Net metering credits available",      "",        "",      "Electricity Rates",     "",      "",         "" },

    { SSC_INPUT,        SSC_NUMBER,      "inflation_rate",           "Inflation rate",                      "%",      "",        "Lifetime",     "*",            "MIN=-99",  "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "ur_price_series",          "Estimated cost of each timestep",     "$",      "",        "Lifetime",     "*",            "",  "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "ur_total_bill",            "Total cost for the calculated period","$",      "",        "Lifetime",     "*",            "",  "" },


var_info_invalid };


	
cm_utilityrateforecast::cm_utilityrateforecast()
{
    add_var_info(vtab_utility_rate_common);
	add_var_info( vtab_utilityrateforecast );
}

bool cm_utilityrateforecast::setup(var_table* vt) {
    m_vartab = vt;
    if (!compute_module::verify("precheck input", SSC_INPUT)) {
        return false;
    }
    dt_hr = as_number("dt_hr");
    analysis_period = as_integer("analysis_period");

    int step_per_year = 8760 / dt_hr;
    int step_per_hour = 1 / dt_hr;

    rate = std::shared_ptr<rate_data>(new rate_data());
    rate_setup::setup(vt, step_per_year, analysis_period, *rate, "cmod_battery");

    /* - Need to determine final locations of pre-processing before writing this
    rate_forecast = std::shared_ptr<UtilityRateForecast>(new UtilityRateForecast(rate.get(), step_per_hour, monthly_net_load, monthly_gen, monthly_gross_load, analysis_period, monthly_peaks));
    rate_forecast->initializeMonth(0, 0);
    rate_forecast->copyTOUForecast();
    */

    return true;
}

void cm_utilityrateforecast::exec( )
{
    /*
    for (size_t hour = 0; hour != 24; hour++)
    {
        for (size_t step = 0; step != _steps_per_hour && idx < _P_load_ac.size(); step++)
        {
            double power = _P_load_ac[idx] - _P_pv_ac[idx];
            // One at a time so we can sort grid points by no-dispatch cost
            std::vector<double> forecast_power = { -power }; // Correct sign convention for cost forecast
            double step_cost = noDispatchForecast->forecastCost(forecast_power, year, (hour_of_year + hour) % 8760, step);
            no_dispatch_cost += step_cost;

            std::vector<double> marginal_power = { -1.0 };
            double marginal_cost = marginalForecast->forecastCost(marginal_power, year, (hour_of_year + hour) % 8760, step);

            grid[count] = grid_point(power, hour, step, step_cost, marginal_cost);
            sorted_grid[count] = grid[count];

            if (debug)
                fprintf(p, "%zu\t %.1f\t %.1f\t %.1f\n", count, _P_load_ac[idx], _P_pv_ac[idx], _P_load_ac[idx] - _P_pv_ac[idx]);

            idx++;
            count++;
        }
    }
    */
}


DEFINE_STATEFUL_MODULE_ENTRY( utilityrateforecast, "Compute the utility rate costs associated with a given rate and time series array of grid usage.", 1 )
