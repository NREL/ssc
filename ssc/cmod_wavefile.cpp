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
#include <string>
#include <fstream>
#include "core.h"
#include "lib_util.h"
#include "lib_weatherfile.cpp"

static var_info _cm_wave_file_reader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                        UNITS     META                      GROUP                 REQUIRED_IF                CONSTRAINTS        UI_HINTS*/
    { SSC_INPUT,         SSC_NUMBER,      "wave_resource_model_choice",           "Hourly or JPD wave resource data",                                 "0/1",             "",             "Weather Reader",          "",                         "INTEGER",                  "" },
    { SSC_INPUT,         SSC_STRING,      "wave_resource_filename",               "local weather file path",                     "",       "",                      "Weather Reader",      "?",                       "LOCAL_FILE",      "" },
    { SSC_INPUT,         SSC_STRING,      "wave_resource_filename_ts",               "local weather file path",                     "",       "",                      "Weather Reader",      "?",                       "LOCAL_FILE",      "" },

    { SSC_INPUT,         SSC_TABLE,       "wave_resource_data",                   "wave resource data from memory",     "",      "",       "Weather Reader",                  "?",                                            "",                "" },
    { SSC_INPUT,         SSC_NUMBER,      "use_specific_wf_wave",               "user specified file",                     "0/1",       "",                      "Weather Reader",      "?=0",                       "INTEGER,MIN=0,MAX=1",      "" },
	
// header data
	{ SSC_OUTPUT,        SSC_STRING,      "name",                    "Name",                                        "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                                        "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                                       "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "country",                 "Country",                                     "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                     "Latitude",                                    "deg",    "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                     "Longitude",                                   "deg",    "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "nearby_buoy_number",      "Nearby buoy number",                          "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "average_power_flux",      "Distance to shore",                           "kW/m",   "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "bathymetry",              "Bathymetry",                                  "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "sea_bed",                 "Sea bed",                                     "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "tz",                      "Time zone",                                   "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "data_source",             "Data source",                                 "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "notes",                   "Notes",                                       "",       "",                      "Weather Reader",      "use_specific_wf_wave=0",                        "",               "" },

																										            
// weather data records																					            
	{ SSC_OUTPUT,        SSC_MATRIX,      "wave_resource_matrix",              "Frequency distribution of resource",                                  "m/s",   "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_OUTPUT,        SSC_ARRAY,       "time_check",                        "Time check",                                                          "",      "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_OUTPUT,        SSC_ARRAY,       "month",                        "Month",                                                          "",      "",                       "Weather Reader",      "?",                        "",                            "" },

    { SSC_OUTPUT,        SSC_ARRAY,       "wave_significant_height",           "Wave height time series data",                                        "m",     "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_OUTPUT,        SSC_ARRAY,       "number_records",                "Number of records in wave time series",                                        "",     "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_OUTPUT,        SSC_ARRAY,       "number_hours",                "Number of hours in wave time series",                                        "",     "",                       "Weather Reader",      "?",                        "",                            "" },

    { SSC_OUTPUT,        SSC_ARRAY,       "wave_energy_period",                "Wave period time series data",                                        "s",     "",                       "Weather Reader",      "?",                        "",                            "" },
var_info_invalid };

class wave_data_provider
{
public:
    enum {
        YEAR, MONTH, DAY, HOUR, MINUTE,
        GHI, DNI, DHI, POA,
        TDRY, TWET, TDEW,
        WSPD, WDIR,
        RH, PRES, SNOW, ALB, AOD,
        _MAXCOL_
    };

    //wave_data_provider();
    //virtual ~wave_data_provider();

    std::string name;
    std::string city;
    std::string state;
    std::string country;
    std::string bathymetry;
    std::string sea_bed;
    std::string data_source;
    std::string notes;
    int year;
    double lat;
    double lon;
    double tz;
    double data_type;
    std::string nearby_buoy_number;
    double average_power_flux;

    std::vector<int> types() { return m_dataid; }
    std::vector<double> wave_heights() { return m_sigwaveheight; }
    std::vector<double> wave_periods() { return m_waveperiod; }
    //std::vector<double> wave_matrix() { return m_wave_resource_matrix_data; }
    util::matrix_t<double> wave_matrix() { return m_wave_resource_matrix_data; }
    std::vector<double> relativeHumidity() { return m_relativeHumidity; }

    

    //virtual bool read_line(std::vector<double>& values) = 0;
    //virtual size_t nrecords() = 0;
    size_t nrecords() { return m_nRecords; }
        
    

    std::string error() { return m_errorMsg; }

    bool has_message() { return m_errorMsg.size() > 0; }
    std::string message() { return m_errorMsg; }

   

    //bool check_hour_of_year(int hour, int line);
    //std::vector<double> read(int datatype, double* wave_height[nrecords], double* wave_period);
    //std::vector<double> read_wave_height(int datatype,)
    // virtual functions specific to weather data source
    /// check if the data is available from weather file


    /// reads one more record
    //virtual bool read(weather_record* r) = 0;

protected:
    /// index of resource type (temp=1,pres=2,speed=3,dir=4) for each measurement height
    std::vector<int> m_dataid;
    size_t m_nRecords;
    /// measurement height corresponding to each column header; same size as m_dataid
    std::vector<double> m_sigwaveheight;
    std::vector<double> m_waveperiod;
    util::matrix_t<double> m_wave_resource_matrix_data;
    //ssc_number_t* m_wave_resource_matrix_data;
    //std::vector<double> m_wave_resource_matrix_data;
    //std::vector<double> m_resourcematrix;
    std::vector<double> m_relativeHumidity;
    std::string m_errorMsg;

    //bool find_closest(int& closest_index, int id, int ncols, double requested_height, int index_to_exclude = -1);
    //bool can_interpolate(int index1, int index2, int ncols, double requested_height);
    
};




class wavedata : public wave_data_provider
{
    size_t irecord;
    util::matrix_t<double> wave_resource_matrix_data;
    std::string stdErrorMsg;
public:
    explicit wavedata(var_data* data_table);
    

    size_t nrecords();

    ssc_number_t get_number(var_data* v, const char* name);

    ssc_number_t* get_vector(var_data* v, const char* name, size_t* len);
    std::string get_string(var_data* v, const char* name);

    bool read_line(std::vector<double>& values);

    std::string get_stdErrorMsg() { return stdErrorMsg; };
};

wavedata::wavedata(var_data* data_table)
{
    irecord = 0;

    stdErrorMsg = "wave data must be an SSC table variable with fields: "
        "(string): name, city, state, country, sea_bed, data_source, notes, "
        "(number): lat, lon, nearby_buoy_number, average_power_flux, bathymetry, tz, "
        "(array): significant_wave_height, wave_energy_period";

    if (data_table->type != SSC_TABLE)
    {
        m_errorMsg = stdErrorMsg;
        return;
    }

   

    lat = get_number(data_table, "lat");
    lon = get_number(data_table, "lon");
    name = get_string(data_table, "name");
    city = get_string(data_table, "city");
    state = get_string(data_table, "state");
    country = get_string(data_table, "country");
    bathymetry = get_string(data_table, "bathymetry");
    sea_bed = get_string(data_table, "sea_bed");
    tz = get_number(data_table, "tz");
    data_source = get_string(data_table, "data_source");
    notes = get_string(data_table, "notes");

    data_type = get_number(data_table, "data_type"); //0-time series data, 1- jpd
    nearby_buoy_number = get_string(data_table, "nearby_buoy_number");
    average_power_flux = get_number(data_table, "average_power_flux");
    //bathymetry = get_number(data_table, "bathymetry"); return string?
    if (data_type == 0) {
        size_t len = 0;
        ssc_number_t* p = get_vector(data_table, "significant_wave_height", &len);
        for (size_t i = 0; i < len; i++)
            m_sigwaveheight.push_back((double)p[i]);

        p = get_vector(data_table, "wave_energy_period", &len);
        for (size_t i = 0; i < len; i++)
            m_waveperiod.push_back((double)p[i]);

        if (m_waveperiod.size() != m_sigwaveheight.size()) {
            m_errorMsg = util::format("number of wave height entries must be same as number of wave period entries");
            return;
        }
    }
    else if (data_type == 1) {
        if (var_data* D = data_table->table.lookup("wave_resource_jpd"))
            if (D->type == SSC_MATRIX)
                m_wave_resource_matrix_data = D->num;

    }
    else {
        m_errorMsg = util::format("must define data type to specify whether wave resource data is in a time series or joint probability distribution");
        return;
    }
    

    
}

size_t wavedata::nrecords()
{
    if (data_type == 0) {
        m_nRecords = m_sigwaveheight.size();
        return m_sigwaveheight.size();
    }
    else {
        m_nRecords = wave_resource_matrix_data.nrows();
        return wave_resource_matrix_data.nrows();
    }
}

ssc_number_t wavedata::get_number(var_data* v, const char* name)
{
    if (var_data* value = v->table.lookup(name))
    {
        if (value->type == SSC_NUMBER)
            return value->num;
    }

    return std::numeric_limits<ssc_number_t>::quiet_NaN();
}

std::string wavedata::get_string(var_data* v, const char* name)
{
    if (var_data* value = v->table.lookup(name))
    {
        if (value->type == SSC_STRING)
            return name;
    }
}

ssc_number_t* wavedata::get_vector(var_data* v, const char* name, size_t* len)
{
    ssc_number_t* p = 0;
    *len = 0;
    if (var_data* value = v->table.lookup(name))
    {
        if (value->type == SSC_ARRAY)
        {
            *len = value->num.length();
            p = value->num.data();
        }
    }
    return p;
}

/*std::string *wavedata::get_string(var_data* v, const char* name)
{
    if (var_data* value = v->table.lookup(name))
    {
        if (value->type == SSC_STRING)
            return value;
    }
}*/

class cm_wave_file_reader : public compute_module
{
public:

	cm_wave_file_reader()
	{
		add_var_info(_cm_wave_file_reader);
	}
	
    void exec()
    {
        smart_ptr<wave_data_provider>::ptr wave_dp;
        size_t nstep = 2920;
        std::string file;
        if (is_assigned("wave_resource_filename") && as_integer("wave_resource_model_choice")==0)
        {
            file = as_string("wave_resource_filename");
        }
        else if (is_assigned("wave_resource_filename_ts") && as_integer("wave_resource_model_choice")==1)
        {
            file = as_string("wave_resource_filename_ts");
            //throw exec_error("wave_file_reader", "Time series filename check");
        }
        else
        {
            throw exec_error("wave_file_reader", "Model choice and wave file do not match");
        }
        if (file.empty())
        {
            throw exec_error("wave_file_reader", "File name missing.");
        }


        std::string buf, buf1;
        std::ifstream ifs(file);

        if (!ifs.is_open())
        {
            throw exec_error("wave_file_reader", "could not open file for reading: " + file);
        }

        std::vector<std::string> values;
        std::vector<std::string> value_0;
        std::vector<std::string> value_1;
        // header if not use_specific_wf_file
        if (as_integer("use_specific_wf_wave") == 0)
        {
            getline(ifs, buf);
            getline(ifs, buf1);

            // header name value pairs
            std::vector<std::string> keys = split(buf);
            values = split(buf1);
            int ncols = (int)keys.size();
            int ncols1 = (int)values.size();

            if (ncols != ncols1 || ncols < 13)
            {
                throw exec_error("wave_file_reader", "incorrect number of header columns: " + std::to_string(ncols));
            }

            assign("name", var_data(values[0]));
            assign("city", var_data(values[1]));
            assign("state", var_data(values[2]));
            assign("country", var_data(values[3]));
            // lat with S is negative
            ssc_number_t dlat = std::numeric_limits<double>::quiet_NaN();
            std::vector<std::string> slat = split(values[4], ' ');
            if (slat.size() > 0)
            {
                dlat = std::stod(slat[0]);
                if (slat.size() > 1)
                {
                    if (slat[1] == "S") dlat = 0.0 - dlat;
                }
            }
            assign("lat", var_data(dlat));
            // lon with W is negative
            ssc_number_t dlon = std::numeric_limits<double>::quiet_NaN();
            std::vector<std::string> slon = split(values[5], ' ');
            if (slon.size() > 0)
            {
                dlon = std::stod(slon[0]);
                if (slon.size() > 1)
                {
                    if (slon[1] == "W") dlon = 0.0 - dlon;
                }
            }
            assign("lon", var_data(dlon));
            assign("nearby_buoy_number", var_data(values[6]));
            assign("average_power_flux", var_data(std::stod(values[7])));
            assign("bathymetry", var_data(values[8]));
            assign("sea_bed", var_data(values[9]));
            assign("tz", var_data(std::stod(values[10])));
            assign("data_source", var_data(values[11]));
            assign("notes", var_data(values[12]));
        }
        // read in 21 rows x 22 columns

        //ssc_number_t* mat = allocate("wave_resource_matrix", 21, 22);
        //size_t numberRecords = 2920;
        //ssc_number_t* wave_heights = allocate("wave_significant_height", numberRecords);
        //ssc_number_t* wave_periods = allocate("wave_energy_period", numberRecords);
        //if (values.size() != 22)
        if (as_integer("wave_resource_model_choice") == 1)
        {
            size_t numberRecords = 0;
            
            while (getline(ifs, buf))
                numberRecords++;
            assign("number_records", (int)numberRecords);
            // rewind the file and reposition right after the header information
            ifs.clear();
            ifs.seekg(0);
            for (size_t i = 0; i < 2; i++)
                getline(ifs, buf);
            ssc_number_t hour0, hour1, hourdiff;
            ssc_number_t ts_significant_wave_height;
            ssc_number_t ts_energy_period;
            size_t ncols = 22;
            size_t nrows = 21;
            size_t sig_wave_height_index = 0;
            size_t energy_period_index = 0;
            ssc_number_t* mat = allocate("wave_resource_matrix", 21, 22);
            //size_t numberRecords = wave_dp->nrecords();
            ssc_number_t* month = allocate("month", numberRecords);
            ssc_number_t* timecheck = allocate("time_check", numberRecords);
            timecheck[0] = 0;
            ssc_number_t* wave_heights = allocate("wave_significant_height", numberRecords);
            ssc_number_t* wave_periods = allocate("wave_energy_period", numberRecords);
            for (size_t r = 0; r < numberRecords; r++) {
                getline(ifs, buf);
                values.clear();
                values = split(buf);
                
                if (r == 0) {
                    //value_0 = split(buf);
                    hour0 = (ssc_number_t)std::stod(values[3]);
                }
                if (r == 1) {
                    //value_1 = split(buf);
                    hour1 = (ssc_number_t)std::stod(values[3]);
                    hourdiff = hour1 - hour0;
                }
                month[r] = (ssc_number_t)std::stod(values[1]);
                timecheck[r] = (ssc_number_t)std::stod(values[3]);
                if (r > 0) {
                    if (timecheck[r] - timecheck[r - 1] != hourdiff && timecheck[r] != 0) {
                        //throw exec_error("wave_file_reader", "Time steps are nonuniform");
                        timecheck[r] = 999;
                    }
                }
                wave_heights[r] = (ssc_number_t)std::stod(values[6]);
                wave_periods[r] = (ssc_number_t)std::stod(values[5]);

                //Make JPD from time series data

                ts_significant_wave_height = wave_heights[r];
                ts_energy_period = wave_periods[r];
                for (size_t j = 0; j < 21; j++) {

                    if (r == 0) mat[(j + 1) * ncols] = (0.25 + j * 0.5);
                    if (abs(ts_significant_wave_height - (0.25 + (j - 1) * 0.5)) <= 0.25 && j != 0) {
                        sig_wave_height_index = j;
                        if (r != 0) break;
                    }
                }
                for (size_t m = 0; m < 22; m++) {
                    if (r == 0) mat[m] = m - 0.5;
                    if (abs(ts_energy_period - (0.5 + (m - 1))) <= 0.5 && m != 0) {
                        energy_period_index = m;
                        if (r != 0) break;
                    }


                }


                //mat[sig_wave_height_index * ncols + energy_period_index] = mat[sig_wave_height_index * ncols + energy_period_index] + 1 / 2920 * 100;
                mat[sig_wave_height_index * ncols + energy_period_index] += 0.0342465753;
                //Set decimal values to 2 for JPD
                if (r == numberRecords - 1) {
                    for (size_t r2 = 0; r2 < 21; r2++) {
                        for (size_t c2 = 0; c2 < 22; c2++) {
                            if (r2 != 0 && c2 != 0) mat[r2 * 22 + c2] = round(mat[r2 * 22 + c2] * 100) / 100;
                        }
                    }
                }
                
            }
            mat[0] = 0;
            assign("number_hours", int(numberRecords * hourdiff));
            return;

        }
        else if (as_integer("wave_resource_model_choice") == 0) {
            ssc_number_t* mat = allocate("wave_resource_matrix", 21, 22);
            for (size_t r = 0; r < 21; r++)
            {
                getline(ifs, buf);
                values.clear();
                values = split(buf);
                if (values.size() != 22)
                {
                    for (size_t c = 0; c < 2; c++)
                    {

                    }
                    //throw exec_error("wave_file_reader", "incorrect number of data columns: " + std::to_string(values.size()));
                }
                for (size_t c = 0; c < 22; c++)
                {
                    if (r == 0 && c == 0)
                        mat[r * 22 + c] = 0.0;
                    else
                        mat[r * 22 + c] = std::stod(values[c]);
                }
            }
        }
        else {
            throw exec_error("wave_file_reader", "Resource data type needs to be defined ");
        }

        //return;

        if (is_assigned("wave_resource_data"))
        {
            wave_dp = std::unique_ptr<wave_data_provider>(new wavedata(lookup("wave_resource_data")));
            if (!wave_dp->error().empty()) {
                throw exec_error("wave_file_reader", wave_dp->error());
                return;
            }
            nstep = wave_dp->nrecords();

            // check for leap day
            bool contains_leap_day = false;
            if (std::fmod((double)nstep, 8784) == 0)
            {
                contains_leap_day = true;
                int leap_steps_per_hr = (int)nstep / 8784;
                log("This weather file appears to contain a leap day. Feb 29th will be skipped. If this is not the case, please check your wind resource file.", SSC_NOTICE);
                nstep = leap_steps_per_hr * 8760;
            }

            /*ze_t steps_per_hour = nstep / 8760;
            if (steps_per_hour * 8760 != nstep && !contains_leap_day)
                throw exec_error("windpower", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nstep));
            */
            //ssc_number_t* mat = allocate("wave_resource_matrix", 21, 22);
            ssc_number_t* mat = allocate("wave_resource_matrix", 21, 22);
            //util::matrix_t<ssc_number_t> &mat = allocate_matrix("wave_resource_matrix", 21, 22);
            ssc_number_t* wave_height = allocate("wave_significant_height", nstep);
            ssc_number_t* wave_period = allocate("wave_period", nstep);
            std::vector<double> wave_height_data = wave_dp->wave_heights();
            std::vector<double> wave_period_data = wave_dp->wave_periods();
            util::matrix_t<double> wave_resource = wave_dp->wave_matrix();
            if (wave_dp->data_type == 0) {
                for (size_t i = 0; i < nstep; i++)
                {
                    wave_height[i] = wave_height_data[i];
                    wave_period[i] = wave_period_data[i];
                }
            }
            else if (wave_dp->data_type == 1)
            {

                for (size_t r = 0; r < 21; r++)
                {


                    for (size_t c = 0; c < 22; c++)
                    {
                        if (r == 0 && c == 0)
                            mat[r * 22 + c] = 0.0;
                        else
                            mat[r * 22 + c] = wave_resource[r * 22 + c];
                    }
                }
            }
            assign("lat", wave_dp->lat);
            assign("lon", wave_dp->lon);
            assign("name", wave_dp->name);
            assign("city", wave_dp->city);
            assign("state", wave_dp->state);
            assign("country", wave_dp->state);
            assign("bathymetry", wave_dp->bathymetry);
            assign("sea_bed", wave_dp->sea_bed);
            assign("tz", wave_dp->tz);
            assign("data_source", wave_dp->data_source);
            assign("notes", wave_dp->notes);
            assign("nearby_buoy_number", wave_dp->nearby_buoy_number);
            assign("average_power_flux", wave_dp->average_power_flux);
            assign("data_type", wave_dp->data_type);
            return;
        }
        else
        {
            return;
        }
    }
};

DEFINE_MODULE_ENTRY(wave_file_reader, "SAM Wave Resource File Reader", 1)
