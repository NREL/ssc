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
    { SSC_INPUT,        SSC_STRING,   "batt_dispatch_pvs_pv_power_file",                  "CSV input pv power profile for testing",         "",       "",      "PV Smoothing",                                        "*",                                  "",                    "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_max_ramp",                 "Maximum ramp rate",                     "% nameplate per ramp interval", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_timestep_multiplier",                   "Weather file timestep multiplier",                       "", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_ub_enable",                  "Enable AC upper bound",                      "0/1", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_lb_enable",                 "Enable AC lower bound",                     "0/1", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_ub",                   "AC upper bound",                       "% nameplate", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ac_lb",                  "AC lower bound",                      "% nameplate", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_short_forecast_enable",                 "Enable forecast",                     "0/1", "",         "PV Smoothing", "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_forecast_shift_periods",                   "Forecast shift periods",                       "", "",         "PV Smoothing", "*",                      "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_energy",                  "Battery energy",                      "kWhac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_power",                  "Battery power",                      "kWac", "",                     "PV Smoothing", "*",                      "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_battery_rte",                  "Round trip efficiency",                      "%", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_curtail_as_control",                  "Curtail as control",                      "0/1", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_curtail_if_violation",                  "Curtail if violation",                      "0/1", "",                     "PV Smoothing", "*",                      "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_kp",                  "kp",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_ki",                  "ki",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_kf",                  "kf",                      "", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_INPUT,        SSC_NUMBER,      "batt_dispatch_pvs_soc_rest",                  "SOC rest",                      "%", "",                     "PV Smoothing", "*",                       "",                         "" },

    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_pv_power",                  "Input PV Power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },
    { SSC_OUTPUT,        SSC_ARRAY,      "batt_dispatch_pvs_pv_power_resampled",                  "Resampled input PV Power",                      "kWac", "",                     "PV Smoothing", "*",                       "",                         "" },

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
        while (getline(ifs, buf) && buf.length() > 0 && ndx < nRecords) {
            auto cols = util::split(buf,",");
            int ncols = (int)cols.size();
            if (ncols != 2) {
                throw exec_error("pv_smoothing", "failed to read 2 columns local test file: " + std::string(file) + " at line "  + std::to_string(ndx));
                return;
            }
            pv_power_input[ndx] = atof(cols[1].c_str());
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

        /*
        if (m_type == WFCSV)
        {
            // if we opened a csv file, it could be SAM/WFCSV format or TMY3
            // try to autodetect a TMY3
            getline(ifs, buf);
            getline(ifs, buf1);
            int ncols = (int)split(buf).size();
            int ncols1 = (int)split(buf1).size();
            size_t num_steps = check_timestep_seconds( t_start, t_end, t_step );

		ssc_number_t *time = allocate("time", num_steps);
		ssc_number_t *timehr = allocate("timehr", num_steps);
		ssc_number_t *month = allocate("month", num_steps);
		ssc_number_t *day = allocate("day", num_steps);
		ssc_number_t *hour = allocate("hour", num_steps);
		ssc_number_t *minute = allocate("minute", num_steps);

		double T = t_start;
		size_t idx = 0;
		while (T < t_end && idx < num_steps)
		{
			double Thr = T / 3600.0;

			time[idx] = (float) T;
			timehr[idx] = (float) Thr;
						
			int m = util::month_of(Thr);
			month[idx] = (ssc_number_t) m ;              // month goes 1-12
			day[idx] = (ssc_number_t) util::day_of_month(m,Thr) ;   // day goes 1-nday_in_month
			hour[idx] = (ssc_number_t) ((int)(Thr)%24);		         // hour goes 0-23
			minute[idx] = (ssc_number_t) ((int)( (Thr-floor(Thr))*60  + t_step/3600.0*30));      // minute goes 0-59
	
			T += t_step;
			idx++;
		}
*/
	}
};

DEFINE_MODULE_ENTRY( pv_smoothing, "PV Smoothing", 1 )
