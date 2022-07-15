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
    { SSC_INPUT,        SSC_NUMBER,      "steps_per_hour",           "Steps per hour",                  "hr",      "",        "Controls",     "*",            "",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "idx",                      "Starting index (lifetime)",               "",        "",        "StatePack",    "",             "",         ""  },

    { SSC_INPUT,        SSC_ARRAY,       "load",                     "Lifetime load forecast",               "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "gen",                      "Lifetime generation forecast",         "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "grid_power",               "Electricity to/from grid",            "",        "",        "Electricity Rates",   "",      "",         "" },

    { SSC_INOUT,        SSC_MATRIX,      "ur_energy_use",            "Energy use or surplus by month and period",      "",        "",      "Electricity Rates",     "",      "",         "" },
    { SSC_INOUT,        SSC_MATRIX,      "ur_dc_peaks",              "Peak demand by month and period",      "",        "",      "Electricity Rates",     "",      "",         "" },

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
    steps_per_hour = as_number("steps_per_hour");
    analysis_period = as_integer("analysis_period");

    int step_per_year = 8760 * steps_per_hour;

    rate = std::shared_ptr<rate_data>(new rate_data());
    rate_setup::setup(vt, step_per_year, analysis_period, *rate, "cmod_utilityrateforecast");

    /* - Need to determine final locations of pre-processing before writing this
    rate_forecast = std::shared_ptr<UtilityRateForecast>(new UtilityRateForecast(rate.get(), steps_per_hour, monthly_net_load, monthly_gen, monthly_gross_load, analysis_period, monthly_peaks));
    rate_forecast->initializeMonth(0, 0);
    rate_forecast->copyTOUForecast();
    */

    return true;
}

void cm_utilityrateforecast::exec( )
{
    // TODO: import nm structure

    std::vector<double> grid_power = m_vartab->as_vector_double("grid_power");
    size_t idx = m_vartab->as_unsigned_long("idx");
    size_t year = idx / (8760 * steps_per_hour);
    size_t index_of_year = idx % 8760;
    size_t hour_of_year = index_of_year / steps_per_hour;
    size_t step = index_of_year % steps_per_hour;

    size_t steps_to_run = grid_power.size();
    size_t index_at_end = idx + steps_to_run;
    size_t count = 0;

    ssc_number_t* cost_at_step = allocate("ur_price_series", steps_to_run);
    ssc_number_t total_bill = 0;
    // TODO: error check vector lengths - ensure we aren't going to overshoot analysis_period

    while (idx < index_at_end)
    {
        // One at a time so we can return cost at each step
        std::vector<double> forecast_power = { grid_power[count] }; // Correct sign convention for cost forecast
        double step_cost = rate_forecast->forecastCost(forecast_power, year, hour_of_year, step);
        total_bill += step_cost;

        cost_at_step[count] = step_cost;

        idx++;
        count++;
        step++;
        if (step >= steps_per_hour) {
            step = 0;
            hour_of_year++;
            if (hour_of_year >= 8760) {
                hour_of_year = 0;
                year++;
            }
        } 
    }
    

}


DEFINE_STATEFUL_MODULE_ENTRY( utilityrateforecast, "Compute the utility rate costs associated with a given rate and time series array of grid usage.", 1 )
