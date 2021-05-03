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

    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_outpower",                  "PV Smoothing output power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_battpower",                  "PV Smoothing battery power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_battsoc",                  "PV Smoothing battery SOC",                      "%", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_curtail",                  "PV Smoothing curtailments",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_violation_list",                  "PV Smoothing violations",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },

    { SSC_OUTPUT,        SSC_NUMBER,      "batt_dispatch_pvs_violation_count",                  "PV Smoothing total violation count",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_NUMBER,      "batt_dispatch_pvs_total_energy",                  "PV Smoothing total energy",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },

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

  //      #conversion factors
        ssc_number_t  power_to_energy_conversion_factor = as_number("batt_dispatch_pvs_ramp_interval") / 60.0;
        ssc_number_t  batt_half_round_trip_eff = sqrt(as_number("batt_dispatch_pvs_battery_rte"));//settings['round_trip_efficiency'] * *0.5


        bool en_forecast = as_boolean("batt_dispatch_pvs_short_forecast_enable");
        ssc_number_t* forecast_pv_energy = allocate("batt_dispatch_pvs_pv_energy_forecast", nRecordsSampled);
        ssc_number_t forecast_shift_periods = as_number("batt_dispatch_pvs_forecast_shift_periods");
        //if (en_forecast) { follow Python code for now.
            for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
                ndx = 0;
                ssc_number_t sum = 0;
                while ((ndx < forecast_shift_periods) && (ndx_sampled+ndx < nRecordsSampled)) {
                    sum += pv_power_input_sampled[ndx_sampled + ndx];
                    ndx++;
                }
                forecast_pv_energy[ndx_sampled] = sum * power_to_energy_conversion_factor;
            }
        //}

        // main loop from ramp_rate_control.py
        ssc_number_t kp = as_number("batt_dispatch_pvs_kp");
        ssc_number_t ki = as_number("batt_dispatch_pvs_ki");
        ssc_number_t kf = as_number("batt_dispatch_pvs_kf");
        ssc_number_t soc_rest = as_number("batt_dispatch_pvs_soc_rest");
        ssc_number_t max_ramp = as_number("batt_dispatch_pvs_max_ramp");

        bool AC_upper_bound_on = as_boolean("batt_dispatch_pvs_ac_ub_enable");
        ssc_number_t AC_upper_bound = as_number("batt_dispatch_pvs_ac_ub");
        bool AC_lower_bound_on = as_boolean("batt_dispatch_pvs_ac_lb_enable");
        ssc_number_t AC_lower_bound = as_number("batt_dispatch_pvs_ac_lb");
        bool curtail_as_control = as_boolean("batt_dispatch_pvs_curtail_as_control");
        bool curtail_if_violation = as_boolean("batt_dispatch_pvs_curtail_if_violation");

        // TODO - SAM battery model
        ssc_number_t battery_energy = as_number("batt_dispatch_pvs_battery_energy");
        ssc_number_t battery_power = as_number("batt_dispatch_pvs_battery_power");


        //#local variables
        ssc_number_t previous_power = 0;
        ssc_number_t battery_soc = 0;
        //#outputs arrays
        ssc_number_t* outpower = allocate("batt_dispatch_pvs_outpower", nRecordsSampled);
        ssc_number_t* battpower = allocate("batt_dispatch_pvs_battpower", nRecordsSampled);
        ssc_number_t* battsoc = allocate("batt_dispatch_pvs_battsoc", nRecordsSampled);
        ssc_number_t* curtail = allocate("batt_dispatch_pvs_curtail", nRecordsSampled);
        ssc_number_t* violation_list = allocate("batt_dispatch_pvs_violation_list", nRecordsSampled);

        // output accumulators
        size_t violation_count = 0;
        ssc_number_t total_energy = 0;

        if (!en_forecast)
            kf = 0;

    //  #iterate through time - series
        for (size_t ndx_sampled = 0; ndx_sampled < nRecordsSampled; ndx_sampled++) {
            ssc_number_t pv_power = pv_power_input_sampled[ndx_sampled];
            ssc_number_t out_power = 0;
            ssc_number_t battery_power_terminal = 0;
            ssc_number_t forecast_power = 0;
//            if (en_forecast)
                forecast_power = forecast_pv_energy[ndx_sampled];
            //            for pv_power, forecast_power in np.nditer([PV_ramp_interval.values, forecast_pv_energy.values]) :
            //                #calculate controller error
            ssc_number_t delta_power = pv_power - previous_power; //#proportional error
            ssc_number_t soc_increment = battery_soc + (pv_power - previous_power) * power_to_energy_conversion_factor;// #integral error
            ssc_number_t future_error = previous_power * forecast_shift_periods * power_to_energy_conversion_factor - forecast_power; //#derivitive error
            ssc_number_t error = kp * delta_power + ki * (soc_increment - soc_rest * battery_energy) - kf * future_error;

            //                #calculate the desired output power, enforce ramp rate limit
            if (error > 0)
                out_power = previous_power + std::min(max_ramp, std::abs(error));
            else
                out_power = previous_power - std::min(max_ramp, std::abs(error));

            //            #enforce grid power limits
            if (AC_upper_bound_on) {
                if (out_power > AC_upper_bound)
                    out_power = AC_upper_bound;
            }
            if (AC_lower_bound_on) {
                if (out_power < AC_lower_bound)
                    out_power = AC_lower_bound;
            }

            //                  #calculate desired(unconstrained) battery power
            battery_power_terminal = out_power - pv_power; //# positive is power leaving battery(discharging)

//                    #adjust battery power to factor in battery constraints
//                    #check SOC limit - reduce battery power if either soc exceeds either 0 or 100 %
//                    #check full
            if ((battery_soc - battery_power_terminal * batt_half_round_trip_eff * power_to_energy_conversion_factor) > battery_energy)
                battery_power_terminal = -1.0 * (battery_energy - battery_soc) / power_to_energy_conversion_factor / batt_half_round_trip_eff; // TODO - check units here - subtrcting soc from battery energy???
//            #check empty
            else if ((battery_soc - battery_power_terminal * power_to_energy_conversion_factor) < 0)
                battery_power_terminal = battery_soc / power_to_energy_conversion_factor / batt_half_round_trip_eff;

            // TODO - use SAM charging and discharging limits (different)
//            #enforce battery power limits
//            #discharging too fast
            if (battery_power_terminal > battery_power)
                battery_power_terminal = battery_power;
            //            #charging too fast
            else if (battery_power_terminal < -1.0 * battery_power)
                battery_power_terminal = -1.0 * battery_power;

            //            #update output power after battery constraints are applied
            out_power = pv_power + battery_power_terminal;

            //            #flag if a ramp rate violation has occurred - up or down - because limits of battery prevented smoothing
            int violation = 0;
            if (std::abs(out_power - previous_power) > (max_ramp + 0.00001))
                violation = 1;


            //                #curtailment
            ssc_number_t curtail_power = 0;

            //#if curtailment is considered part of the control - don't count up-ramp violations
            if (curtail_as_control) {
                if ((out_power - previous_power) > (max_ramp - 0.00001)) {
                    out_power = previous_power + max_ramp;// #reduce output to a non - violation
                    curtail_power = pv_power + battery_power_terminal - out_power;// #curtail the remainder
                    violation = 0;
                }
            }

            //            #with this setting, curtail output power upon an upramp violation - rather than sending excess power to the grid
            //            #curtailment still counts as a violation
            //            #sum total of energy output is reduced
            if (curtail_if_violation) {
                if ((out_power - previous_power) > (max_ramp - 0.00001)) {
                    out_power = previous_power + max_ramp; //#reduce output to a non - violation
                    curtail_power = pv_power + battery_power_terminal - out_power;// #curtail the remainder
                }
            }


            //           #update memory variables
            if (battery_power_terminal > 0)//:#discharging - efficiency loss increases the amount of energy drawn from the battery
                battery_soc = battery_soc - battery_power_terminal * power_to_energy_conversion_factor / batt_half_round_trip_eff;
            else if (battery_power_terminal < 0)//:#charging - efficiency loss decreases the amount of energy put into the battery
                battery_soc = battery_soc - battery_power_terminal * batt_half_round_trip_eff * power_to_energy_conversion_factor;
            previous_power = out_power;

            //                #update output variables
            total_energy += out_power;
            violation_count += violation;

            outpower[ndx_sampled] = out_power;
            battpower[ndx_sampled] = battery_power_terminal;
            battsoc[ndx_sampled] = battery_soc;
            violation_list[ndx_sampled] = violation;
            curtail[ndx_sampled] = curtail_power;
        }
//        #post - processing
        assign("batt_dispatch_pvs_violation_count", (var_data)(ssc_number_t)violation_count);
        assign("batt_dispatch_pvs_total_energy", (var_data)(ssc_number_t)(total_energy* power_to_energy_conversion_factor));
 
	}
};

DEFINE_MODULE_ENTRY( pv_smoothing, "PV Smoothing", 1 )
