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
#include "lib_util.h"
#include <fstream>


static var_info _cm_vtab_pv_smoothing[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
    { SSC_INPUT,        SSC_STRING,      "batt_dispatch_pvs_pv_power_file",                  "CSV input pv power profile for testing",         "",       "",      "PV Smoothing",                                        "*",                                  "",                    "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_max_ramp",                 "Maximum ramp rate",                     "% nameplate per ramp interval", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_timestep_multiplier",                   "Weather file timestep multiplier",                       "", "",         "PV Smoothing", "*",                       "",                     "" },
    // calculated values that is = batt_dispatch_pvs_timestep_multiplier * gen input timestep
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ramp_interval",                   "Remp interval",                       "minutes", "",         "PV Smoothing", "*",                       "",                     "" },

    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_ub_enable",                  "Enable AC upper bound",                      "0/1", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_lb_enable",                 "Enable AC lower bound",                     "0/1", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_ub",                   "AC upper bound",                       "% nameplate", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_lb",                  "AC lower bound",                      "% nameplate", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_short_forecast_enable",                 "Enable forecast",                     "0/1", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_forecast_shift_periods",                   "Forecast shift periods",                       "", "",         "PV Smoothing", "*",                      "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_energy",                  "Battery energy",                      "kWhac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_power",                  "Battery power",                      "kWac", "",                     "PV Smoothing", "*",                      "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_nameplate_ac",                  "Nameplate",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_interconnection_limit",                  "Interconnection limit",                      "kWac", "",                     "PV Smoothing", "*",                      "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_rte",                  "Round trip efficiency",                      "%", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_curtail_as_control",                  "Curtail as control",                      "0/1", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_curtail_if_violation",                  "Curtail if violation",                      "0/1", "",                     "PV Smoothing", "*",                      "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_kp",                  "kp",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ki",                  "ki",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_kf",                  "kf",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_soc_rest",                  "SOC rest",                      "%", "",                     "PV Smoothing", "*",                       "",                         "" },

    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_pv_power",                  "Input PV Power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_pv_power_resampled",                  "Resampled input PV Power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_pv_energy_forecast",                  "Perfect energy forecast of input PV Power",                      "kWhac", "",                     "PV Smoothing", "*",                       "",                         "" },

var_info_invalid };

class cm_pv_smoothing : public compute_module
{
private:
public:
	cm_pv_smoothing()
	{
		add_var_info(_cm_vtab_pv_smoothing);
	}

	void exec( )
	{
    /* Note that the file will be replaced by gen in the library and this file will be a specific test case */
        std::string file = as_string("batt_dispatch_pvs_pv_power_file");
        std::string buf, buf1;
        std::ifstream ifs(file);

        if (!ifs.is_open())
        {
            throw exec_error("pv_smoothing", "failed to read local test file: " + std::string(file));
            return;
        }
        // header columns
        getline(ifs, buf);
        // number of records
        size_t nRecords = 0; // figure out how many records there are

        while (getline(ifs, buf) && buf.length() > 0)
            nRecords++;


        // reposition to where we were
        ifs.clear();
        ifs.seekg(0);
        getline(ifs, buf);  // header names

        ssc_number_t* pv_power_input = allocate("batt_dispatch_pvs_pv_power", nRecords);
        size_t ndx = 0;

        // lambda function to split Python sample file that has delimiters in timestamps
        auto splitfunc = [](std::string& str, std::string& delim) {
            std::vector< std::string > list;

            char cur_delim[2] = { 0,0 };
            std::string::size_type m_pos = 0;
            std::string token;

            while (m_pos < str.length())
            {
                std::string::size_type pos = str.find(delim, m_pos);
                if (pos == std::string::npos)
                {
                    cur_delim[0] = 0;
                    token.assign(str, m_pos, std::string::npos);
                    m_pos = str.length();
                }
                else
                {
                    cur_delim[0] = str[pos];
                    std::string::size_type len = pos - m_pos;
                    token.assign(str, m_pos, len);
                    m_pos = pos + delim.length();
                }

                 list.push_back(token);
            }

            return list;

        };

        std::string delim = "\",";
        // combining to match with Python code
        //    df['Power_scaled'] = df['Power'].divide(500) #normalize by the AC nameplate rating
        ssc_number_t batt_dispatch_pvs_nameplate_ac = as_number("batt_dispatch_pvs_nameplate_ac");

        while (getline(ifs, buf) && buf.length() > 0 && ndx < nRecords) {
//            auto cols = util::split(buf, "\","); // Cheat for now since Python timestamps include "," delimiter
            auto cols = splitfunc(buf,delim); // Cheat for now since Python timestamps include "," delimiter
            int ncols = (int)cols.size();
            if (ncols != 2) {
                throw exec_error("pv_smoothing", "failed to read 2 columns local test file: " + std::string(file) + " at line "  + std::to_string(ndx));
                return;
            }
            pv_power_input[ndx] = atof(cols[1].c_str()) / batt_dispatch_pvs_nameplate_ac;
            ndx++;
        }

        size_t timestep_multiplier = as_integer("batt_dispatch_pvs_timestep_multiplier");
        size_t nRecordsSampled = nRecords / timestep_multiplier;
        ssc_number_t* pv_power_input_sampled = allocate("batt_dispatch_pvs_pv_power_resampled", nRecordsSampled);

        ndx = 0;
        for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
            ssc_number_t sum = 0;
            for (size_t i_sampled = 0; i_sampled < timestep_multiplier && ndx < nRecords; i_sampled++) {
                sum += pv_power_input[ndx];
                ndx++;
            }
            pv_power_input_sampled[ndx_sampled] = sum / timestep_multiplier;
        }

        // TODO only calculate if forecast enabled
        if (as_boolean("batt_dispatch_pvs_short_forecast_enable")) {
            ssc_number_t forecast_shift_periods = as_number("batt_dispatch_pvs_forecast_shift_periods");
            ssc_number_t ramp_interval = as_number("batt_dispatch_pvs_ramp_interval")/60.0; // minutes to hour
            ssc_number_t* forecast_pv_energy = allocate("batt_dispatch_pvs_pv_energy_forecast", nRecordsSampled);
            for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
                ndx = 0;
                ssc_number_t sum = 0;
                while ((ndx < forecast_shift_periods) && (ndx_sampled+ndx < nRecordsSampled)) {
                    sum += pv_power_input_sampled[ndx_sampled + ndx];
                    ndx++;
                }
                forecast_pv_energy[ndx_sampled] = sum * ramp_interval;
            }
        }

	}
};

DEFINE_MODULE_ENTRY( pv_smoothing, "PV Smoothing", 1 )
