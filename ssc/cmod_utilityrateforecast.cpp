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

#include "cmod_utilityrateforecast.h"

static var_info vtab_utilityrateforecast[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                                  UNITS     META       GROUP           REQUIRED_IF     CONSTRAINTS UI_HINTS*/
    { SSC_INPUT,        SSC_NUMBER,      "dt_hr",                    "Time step in hours",                  "hr",      "",        "Controls",     "*",            "",         "" },
    { SSC_INPUT,        SSC_NUMBER,      "last_idx",                 "Last index (lifetime)",               "",        "",        "StatePack",    "",             "",         ""  },

    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_load",          "Monthly load forecast",               "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_gen",           "Monthly generation forecast",         "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "ur_monthly_peaks",         "Monthly peaks forecast",              "",        "",        "Electricity Rates",   "",      "",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "grid_power",               "Electricity to/from grid",            "",        "",        "Electricity Rates",   "",      "",         "" },

    { SSC_INOUT,        SSC_MATRIX,      "ur_net_metering_credits",  "Net metering credits available",      "",        "",      "Electricity Rates",     "",      "",         "" },

    { SSC_INPUT,        SSC_NUMBER,      "inflation_rate",           "Inflation rate",                      "%",      "",        "Lifetime",     "*",            "MIN=-99",  "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "ur_price_series",          "Estimated cost of each timestep",     "$",      "",        "Lifetime",     "*",            "MIN=-99",  "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "ur_total_bill",            "Total cost for the calculated period","$",      "",        "Lifetime",     "*",            "MIN=-99",  "" },


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

    return true;
}

void cm_utilityrateforecast::exec( )
{
			
}


DEFINE_STATEFUL_MODULE_ENTRY( utilityrateforecast, "Compute the utility rate costs associated with a given rate and time series array of grid usage.", 1 )
