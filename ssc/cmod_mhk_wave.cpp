/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "core.h"
#include "common.h"
#include <vector>
#include <algorithm> 

static var_info _cm_vtab_mhk_wave[] = {
	//   VARTYPE			DATATYPE			NAME									LABEL																UNITS           META            GROUP              REQUIRED_IF					CONSTRAINTS					UI_HINTS	
	{ SSC_INPUT,            SSC_NUMBER,         "wave_resource_model_choice",           "Hourly or JPD wave resource data",                                 "0/1",             "",             "MHKWave",          "",                         "INTEGER",                  "" },
    { SSC_INPUT,			SSC_MATRIX,			"wave_resource_matrix",					"Frequency distribution of wave resource as a function of Hs and Te","",			"",             "MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    //{ SSC_INPUT,            SSC_MATRIX,         "wave_resource_time_series",            "Time series (3 hour?) wave resource data",                         "",             "",             "MHKWave",          "wave_resource_model_choice=1","",                      "" },
    { SSC_INPUT,            SSC_ARRAY,          "wave_significant_height",              "Significant wave height time series data",                         "m",            "",             "MHKWave",          "wave_resource_model_choice=1", "",                 ""   },
    { SSC_INPUT,            SSC_ARRAY,          "wave_energy_period",                   "Wave period time series data",                                     "s",            "",             "MHKWave",          "wave_resource_model_choice=1", "",                 ""   },
    { SSC_INPUT,			SSC_MATRIX,			"wave_power_matrix",					"Wave Power Matrix",												"",				"",             "MHKWave",			"*",						"",							"" },
//	{ SSC_INPUT,			SSC_NUMBER,			"annual_energy_loss",					"Total energy losses",												"%",			"",             "MHKWave",			"?=0",						"",							"" },
	//{ SSC_INPUT,			SSC_NUMBER,			"calculate_capacity",					"Calculate capacity outside UI?",									"0/1",			"",             "MHKWave",          "?=1",                      "INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT,			SSC_NUMBER,			"number_devices",						"Number of wave devices in the system",								"",				"",             "MHKWave",          "?=1",                      "INTEGER",			    	"" },
	{ SSC_INPUT,			SSC_NUMBER,			"system_capacity",						"System Nameplate Capacity",										"kW",			"",				"MHKWave",			"?=0",						"",							"" },
    { SSC_INPUT,           SSC_ARRAY,           "number_hours",                "Number of hours in wave time series",                                        "",     "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_INPUT,           SSC_ARRAY,           "number_records",                "Number of records in wave time series",                                        "",     "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_INPUT,           SSC_ARRAY,           "month",                        "Month",                                                          "",      "",                       "Weather Reader",      "?",                        "",                            "" },
    { SSC_INPUT,          SSC_ARRAY,           "time_check",                        "Time check",                                                          "",      "",                       "Weather Reader",      "?",                        "",                            "" },

	{ SSC_INPUT,			SSC_NUMBER,			"device_rated_power",				"Rated capacity of device",													"kW",			"",				"MHKWave",			"*",		"",						"" },
    { SSC_INPUT,			SSC_NUMBER,			"fixed_charge_rate",						"FCR from LCOE Cost page",									"",				"",             "MHKWave",         "?=1",                      "",				"" },
    { SSC_INPUT,			SSC_NUMBER,			"device_costs_total",						"Device costs",									"$",				"",             "MHKWave",         "?=1",                      "",				"" },
    { SSC_INPUT,			SSC_NUMBER,			"balance_of_system_cost_total",						"BOS costs",									"$",				"",             "MHKWave",         "?=1",                      "",				"" },
    { SSC_INPUT,			SSC_NUMBER,			"financial_cost_total",						"Financial costs",									"$",				"",             "MHKWave",         "?=1",                      "",				"" },
    { SSC_INPUT,			SSC_NUMBER,			"total_operating_cost",						"O&M costs",									"$",				"",             "MHKWave",         "?=1",                      "",				"" },
	// losses
	{ SSC_INPUT,			SSC_NUMBER,			"loss_array_spacing",				"Array spacing loss",													"%",			"",				"MHKWave",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_resource_overprediction",				"Resource overprediction loss",													"%",			"",				"MHKWave",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_transmission",				"Transmission losses",													"%",			"",				"MHKWave",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_downtime",				"Array/WEC downtime loss",													"%",			"",				"MHKWave",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_additional",				"Additional losses",													"%",			"",				"MHKWave",			"*",		"",						"" },



	{ SSC_OUTPUT,			SSC_NUMBER,			"device_average_power",					"Average power production of a single device",											"kW",			"",				"MHKWave",			"*",						"",							"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"annual_energy",						"Annual energy production of array",											"kWh",			"",				"MHKWave",			"*",						"",							"" },
    { SSC_OUTPUT,           SSC_ARRAY,          "hourly_energy",                        "Hourly energy production of array",                                            "kWh",          "", "Time Series",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,           SSC_ARRAY,          "hourly_energy_interp",                        "Hourly energy production of array (8760)",                                            "kWh",          "", "Time Series",          "wave_resource_model_choice=1",                        "",          "" },

    { SSC_OUTPUT,           SSC_ARRAY,          "sig_wave_height_index_mat",            "Wave height index locations for time series",                      "m",                         "", "MHKWave",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,           SSC_ARRAY,          "sig_wave_height_index_mat_interp",            "Wave height index locations for time series",                      "m",                         "", "Time Series",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,           SSC_ARRAY,          "sig_wave_height_index_location",            "Wave height index number for time series",                      "m",                         "", "MHKWave",          "wave_resource_model_choice=1",                        "",          "" },

    { SSC_OUTPUT,           SSC_ARRAY,          "energy_period_index_mat",            "Wave period index locations for time series",                      "s",                         "", "MHKWave",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,           SSC_ARRAY,          "energy_period_index_mat_interp",            "Wave period index locations for time series",                      "s",                         "", "Time Series",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,           SSC_ARRAY,          "energy_period_index_location",            "Wave period index number for time series",                      "s",                         "", "MHKWave",          "wave_resource_model_choice=1",                        "",          "" },

    { SSC_OUTPUT,           SSC_ARRAY,          "wave_power_index_mat",            "Wave power for time series",                      "kW",                         "", "MHKWave",          "wave_resource_model_choice=1",                        "",          "" },
    { SSC_OUTPUT,			SSC_NUMBER,			"capacity_factor",						"Capacity Factor",													"%",			"",				"MHKWave",			"*",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"numberRecords",						"Number of Records",													"",			"",				"MHKWave",			"",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"numberHours",						"Number of Hours",													"",			"",				"MHKWave",			"",						"",							"" },

    { SSC_OUTPUT,			SSC_MATRIX,			"annual_energy_distribution",			"Annual energy production as function of Hs and Te",				"",				"",				"MHKWave",			"",						"",							"" },
    { SSC_OUTPUT,			SSC_MATRIX,			"annual_energy_distribution_time",			"Annual energy production as function of Time",				"",				"",				"MHKWave",			"",						"",							"" },

    { SSC_OUTPUT,			SSC_NUMBER,			"wave_resource_start_height",			"Wave height at which first non-zero wave resource value occurs (m)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_resource_start_period",			"Wave period at which first non-zero wave resource value occurs (s)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_resource_end_height",			"Wave height at which last non-zero wave resource value occurs (m)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_resource_end_period",			"Wave period at which last non-zero wave resource value occurs (s)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_power_start_height",			"Wave height at which first non-zero WEC power output occurs (m)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_power_start_period",			"Wave period at which first non-zero WEC power output occurs (s)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_power_end_height",			"Wave height at which last non-zero WEC power output occurs (m)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"wave_power_end_period",			"Wave period at which last non-zero WEC power output occurs (s)",				"",				"",				"MHKWave",			"wave_resource_model_choice=0",						"",							"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_capital_cost_kwh",           "Capital costs per unit annual energy",		"$/kWh",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_device_cost_kwh",            "Device costs per unit annual energy",		"$/kWh",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_bos_cost_kwh",               "Balance of system costs per unit annual energy",		"$/kWh",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_financial_cost_kwh",         "Financial costs per unit annual energy",		"$/kWh",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_om_cost_kwh",                "O&M costs per unit annual energy",		"$/kWh",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_capital_cost_lcoe",          "Capital cost as percentage of overall LCOE",		"%",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_device_cost_lcoe",           "Device cost",		"%",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_bos_cost_lcoe",              "BOS cost",		"%",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_financial_cost_lcoe",        "Financial cost",		"%",			"",				"MHKWave",			"*",						"",						"" },
    { SSC_OUTPUT,			SSC_NUMBER,			"total_om_cost_lcoe",               "O&M cost (annual)",		"%",			"",				"MHKWave",			"*",						"",						"" },
    var_info_invalid
};

class cm_mhk_wave : public compute_module
{
private:
public:
	cm_mhk_wave() {
		add_var_info(_cm_vtab_mhk_wave);
	}

	void exec() {


        // total loss
        double total_loss = as_double("loss_array_spacing")
            + as_double("loss_resource_overprediction")
            + as_double("loss_transmission")
            + as_double("loss_downtime")
            + as_double("loss_additional");


        util::matrix_t<double>  wave_power_matrix = as_matrix("wave_power_matrix");
        //Get the system capacity
        //double system_capacity = as_double("system_capacity");
        double annual_energy = 0, device_rated_capacity = 0, device_average_power = 0, capacity_factor = 0;
        //User either sets device_rated_capacity in the UI, or allows cmod to determine from power curve:
        device_rated_capacity = as_double("device_rated_power");

        //Read number of devices
        int number_devices = as_integer("number_devices");
        
        //Read and store wave resource and power matrix as a 2D matrix of vectors:
        util::matrix_t<double>  wave_resource_matrix = as_matrix("wave_resource_matrix");

        if(as_integer("wave_resource_model_choice")==0) {

            //util::matrix_t<double>  wave_resource_matrix = as_matrix("wave_resource_matrix");
		    
		    //Check to ensure size of wave_power_matrix == wave_resource_matrix :
		    if ( (wave_resource_matrix.ncols() !=  wave_power_matrix.ncols() ) || ( wave_resource_matrix.nrows() != wave_power_matrix.nrows() ) )
			    throw exec_error("mhk_wave", "Size of Power Matrix is not equal to Wave Resource Matrix");

		    //Checker to ensure frequency distribution adds to >= 99.5%:
		    double resource_vect_checker = 0;

		    //Allocate memory to store annual_energy_distribution:
		    ssc_number_t *p_annual_energy_dist = allocate("annual_energy_distribution", wave_resource_matrix.nrows(), wave_resource_matrix.ncols());
            
		    int k = 0;
		
		
		    //Create a vector to store 1st column values of resource and power curve. This is compared against
		    //the values of 1st column passed by user in UI:
		    //std::vector<double> _check_column{0, 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.25, 7.75, 8.25, 8.75, 9.25, 9.75};
		
		    for (size_t i = 0; i < (size_t)wave_power_matrix.nrows(); i++) {
			    for (size_t j = 0; j < (size_t)wave_power_matrix.ncols(); j++) {

				    //Store max power if not set in UI:
				    /*if(as_integer("calculate_capacity") > 0)
					    if (_power_vect[i][j] > system_capacity)
						    system_capacity = _power_vect[i][j];*/

				    //Calculate and allocate annual_energy_distribution:
				    if (j == 0 || i == 0)	//Where (i = 0) is the row header, and (j =  0) is the column header.
					    p_annual_energy_dist[k] = (ssc_number_t) wave_resource_matrix.at(i, j);
				    else {
					    p_annual_energy_dist[k] = (ssc_number_t)(wave_resource_matrix.at(i,j) * wave_power_matrix.at(i,j) * 8760.0 / 100.0);
					    annual_energy += p_annual_energy_dist[k];
					    device_average_power += (p_annual_energy_dist[k] / 8760);
					    //Checker to ensure frequency distribution adds to >= 99.5%:
					    resource_vect_checker += wave_resource_matrix.at(i,j);
				    }
				    k++;

			    }

			    /*//Throw exception if default header column (of power curve and resource) does not match user input header row:
			    if (_check_column[i] != wave_resource_matrix.at(i, 0))
				    throw compute_module::exec_error("mhk_wave", "Wave height bins of resource matrix don't match. Reset bins to default");
			    if (_check_column[i] != wave_power_matrix.at(i,0))
				    throw compute_module::exec_error("mhk_wave", "Wave height bins of power matrix don't match. Reset bins to default");*/
		    }

            if (resource_vect_checker < 99.5)
                throw exec_error("mhk_wave", "Probability vector does not add up to 100%.");


            double wave_resource_start_period = 0;
            double wave_resource_start_height = 0;
            double wave_resource_end_period = 0;
            double wave_resource_end_height = 0;
            for (size_t l = 0; l < (size_t)wave_power_matrix.nrows(); l++) {
                for (size_t m = 0; m < (size_t)wave_power_matrix.ncols(); m++) {

                    //Store max power if not set in UI:
                    /*if(as_integer("calculate_capacity") > 0)
                        if (_power_vect[i][j] > system_capacity)
                            system_capacity = _power_vect[i][j];*/

                            //Calculate and allocate annual_energy_distribution:
                    if ((ssc_number_t)wave_resource_matrix.at(l, m) != 0 && (ssc_number_t)wave_resource_matrix.at(l, m - 1) == 0 && (ssc_number_t)wave_resource_matrix.at(l, m + 1) != 0 && (m - 1) != 0)
                    {
                        if (wave_resource_start_period == 0)
                        {
                            wave_resource_start_period = wave_resource_matrix.at(0, m);
                            wave_resource_start_height = wave_resource_matrix.at(l, 0);
                        }
                    }
                    else if ((ssc_number_t)wave_resource_matrix.at(l, m) != 0 && (ssc_number_t)wave_resource_matrix.at(l, m - 1) != 0 && (ssc_number_t)wave_resource_matrix.at(l, m + 1) == 0 && (m - 1) != 0)
                    {
                        wave_resource_end_period = wave_resource_matrix.at(0, m);
                        wave_resource_end_height = wave_resource_matrix.at(l, 0);
                    }
                    else
                    {

                    }


                }

                /*//Throw exception if default header column (of power curve and resource) does not match user input header row:
                if (_check_column[i] != wave_resource_matrix.at(i, 0))
                    throw compute_module::exec_error("mhk_wave", "Wave height bins of resource matrix don't match. Reset bins to default");
                if (_check_column[i] != wave_power_matrix.at(i,0))
                    throw compute_module::exec_error("mhk_wave", "Wave height bins of power matrix don't match. Reset bins to default");*/
            }
            double wave_power_start_period = 0;
            double wave_power_start_height = 0;
            double wave_power_end_period = 0;
            double wave_power_end_height = 0;
            for (size_t n = 0; n < (size_t)wave_power_matrix.nrows(); n++) {
                for (size_t p = 0; p < (size_t)wave_power_matrix.ncols(); p++) {

                    //Store max power if not set in UI:
                    /*if(as_integer("calculate_capacity") > 0)
                        if (_power_vect[i][j] > system_capacity)
                            system_capacity = _power_vect[i][j];*/

                            //Calculate and allocate annual_energy_distribution:
                    if ((ssc_number_t)wave_power_matrix.at(n, p) != 0 && (ssc_number_t)wave_power_matrix.at(n, p - 1) == 0 && (ssc_number_t)wave_power_matrix.at(n, p + 1) != 0 && (p - 1) != 0)
                    {
                        if (wave_power_start_period == 0)
                        {
                            wave_power_start_period = wave_power_matrix.at(0, p);
                            wave_power_start_height = wave_power_matrix.at(n, 0);
                        }
                    }
                    else if ((ssc_number_t)wave_power_matrix.at(n, p) != 0 && (ssc_number_t)wave_power_matrix.at(n, p - 1) != 0 && (p - 1) != 0 && (ssc_number_t)wave_power_matrix.at(n, p + 1) == 0)
                    {
                        wave_power_end_period = wave_power_matrix.at(0, p);
                        wave_power_end_height = wave_power_matrix.at(n, 0);
                    }
                    else
                    {

                    }


                }

                /*//Throw exception if default header column (of power curve and resource) does not match user input header row:
                if (_check_column[i] != wave_resource_matrix.at(i, 0))
                    throw compute_module::exec_error("mhk_wave", "Wave height bins of resource matrix don't match. Reset bins to default");
                if (_check_column[i] != wave_power_matrix.at(i,0))
                    throw compute_module::exec_error("mhk_wave", "Wave height bins of power matrix don't match. Reset bins to default");*/
            }

            assign("wave_resource_start_height", var_data((ssc_number_t)wave_resource_start_height));
            assign("wave_resource_end_height", var_data((ssc_number_t)wave_resource_end_height));
            assign("wave_resource_start_period", var_data((ssc_number_t)wave_resource_start_period));
            assign("wave_resource_end_period", var_data((ssc_number_t)wave_resource_end_period));
            assign("wave_power_start_height", var_data((ssc_number_t)wave_power_start_height));
            assign("wave_power_end_height", var_data((ssc_number_t)wave_power_end_height));
            assign("wave_power_start_period", var_data((ssc_number_t)wave_power_start_period));
            assign("wave_power_end_period", var_data((ssc_number_t)wave_power_end_period));

            capacity_factor = annual_energy / (device_rated_capacity * number_devices * 8760);

        }

        if (as_integer("wave_resource_model_choice")==1) {

            int number_records = as_integer("number_records");
            int number_hours = as_integer("number_hours");
            std::vector<double> wave_height_input = as_vector_double("wave_significant_height");
            std::vector<double> wave_period_input = as_vector_double("wave_energy_period");
            std::vector<double> month = as_vector_double("month");
            std::vector<double> hour = as_vector_double("time_check");
            ssc_number_t* energy_hourly = allocate("hourly_energy", number_records);
            ssc_number_t* energy_hourly_interp = allocate("hourly_energy_interp", number_hours);
            ssc_number_t* sig_wave_height_index_mat = allocate("sig_wave_height_index_mat", number_records);
            ssc_number_t* sig_wave_height_index_location = allocate("sig_wave_height_index_location", number_records);
            ssc_number_t* sig_wave_height_index_mat_interp = allocate("sig_wave_height_index_mat_interp", number_hours);
            ssc_number_t* energy_period_index_mat = allocate("energy_period_index_mat", number_records);
            ssc_number_t* energy_period_index_location = allocate("energy_period_index_location", number_records);
            ssc_number_t* energy_period_index_mat_interp = allocate("energy_period_index_mat_interp", number_hours);
            ssc_number_t* wave_power_index_mat = allocate("wave_power_index_mat", number_records);
            ssc_number_t* p_annual_energy_dist = allocate("annual_energy_distribution", wave_power_matrix.nrows(), wave_power_matrix.ncols());
            ssc_number_t* p_annual_energy_dist_time = allocate("annual_energy_distribution_time", 13, 9);
            double ts_significant_wave_height, ts_energy_period;
            
            int hour_step = number_hours / number_records;
            /* Bilinear interpolation of power matrix for time series analysis*/
            double Q11, Q12, Q21, Q22;
            double x1, x2, y1, y2;
            double a0, a1, a2, a3;
            Q11 = wave_power_matrix.at(1, 1);
            Q12 = wave_power_matrix.at(1, wave_power_matrix.ncols() - 1);
            Q21 = wave_power_matrix.at(wave_power_matrix.nrows() - 1, 1);
            Q22 = wave_power_matrix.at(wave_power_matrix.nrows() - 1, wave_power_matrix.ncols() - 1);
            x1 = wave_power_matrix.at(1, 0);
            x2 = wave_power_matrix.at(wave_power_matrix.nrows() - 1, 0);
            y1 = wave_power_matrix.at(0, 1);
            y2 = wave_power_matrix.at(0, wave_power_matrix.ncols() - 1);

            a0 = (Q11 * x2 * y2) / ((x1 - x2) * (y1 - y2)) + (Q12 * x2 * y1) / ((x1 - x2) * (y2 - y1)) + (Q21 * x1 * y2) / ((x1 - x2) * (y2 - y1)) + (Q22 * x1 * y1) / ((x1 - x2) * (y1 - y2));
            a1 = (Q11 * y2) / ((x1 - x2) * (y2 - y1)) + (Q12 * y1) / ((x1 - x2) * (y1 - y2)) + (Q21 * y2) / ((x1 - x2) * (y1 - y2)) + (Q22 * y1) / ((x1 - x2) * (y2 - y1));
            a2 = (Q11 * x2) / ((x1 - x2) * (y2 - y1)) + (Q12 * x1) / ((x1 - x2) * (y1 - y2)) + (Q21 * x2) / ((x1 - x2) * (y1 - y2)) + (Q22 * x1) / ((x1 - x2) * (y2 - y1));
            a3 = (Q11) / ((x1 - x2) * (y1 - y2)) + (Q12) / ((x1 - x2) * (y2 - y1)) + (Q21) / ((x1 - x2) * (y2 - y1)) + (Q22) / ((x1 - x2) * (y1 - y2));

            
            ssc_number_t sig_wave_height_index = 0;
            ssc_number_t energy_period_index = 0;
            for (size_t i = 0; i < number_records; i++) {
                ts_significant_wave_height = wave_height_input[i];
                ts_energy_period = wave_period_input[i];
                for (ssc_number_t j = 0; j < (ssc_number_t)wave_resource_matrix.nrows(); j++) {
                    if (abs(ts_significant_wave_height - wave_resource_matrix.at(size_t(j), 0)) < 0.25) {
                        sig_wave_height_index = j;
                        sig_wave_height_index_mat[i] = sig_wave_height_index;
                        
                       
                    }
                    if (i == 0) p_annual_energy_dist[size_t(j) * 22] = wave_resource_matrix.at(j, 0);
                }
                for (ssc_number_t m = 0; m < (ssc_number_t)wave_resource_matrix.ncols(); m++) {
                    if (abs(ts_energy_period - wave_resource_matrix.at(0, size_t(m))) < 0.50) {
                        energy_period_index = m;
                        energy_period_index_mat[i] = energy_period_index;
                    }
                    if (i == 0) p_annual_energy_dist[size_t(m)] = wave_resource_matrix.at(0, m);
                }

            
                energy_hourly[i] = (ssc_number_t)(wave_power_matrix.at(size_t(sig_wave_height_index), size_t(energy_period_index))) * hour_step;
                p_annual_energy_dist[size_t(sig_wave_height_index) * 22 + size_t(energy_period_index)] += energy_hourly[i];
                
                energy_hourly_interp[i*3] = energy_hourly[i];
                energy_hourly_interp[i*3 + 1] = energy_hourly[i];
                energy_hourly_interp[i*3 + 2] = energy_hourly[i];
                for (size_t m = 0; m < 13; m++) {
                    for (size_t h = 0; h < 9; h++) {
                        if (i == 0) {
                            p_annual_energy_dist_time[m * 9] = m;
                            p_annual_energy_dist_time[h] = 3*(h-1);
                        }
                        if (month[i] == m && hour[i] == 3*(h-1)) {
                            p_annual_energy_dist_time[m * 9 + h] += energy_hourly[i];
                            break;
                        }
                    }
                }
                sig_wave_height_index_mat[i] = (ssc_number_t)(wave_power_matrix.at(size_t(sig_wave_height_index), 0));
                sig_wave_height_index_mat_interp[i * 3] = sig_wave_height_index_mat[i];
                sig_wave_height_index_mat_interp[i * 3 + 1] = sig_wave_height_index_mat[i];
                sig_wave_height_index_mat_interp[i * 3 + 2] = sig_wave_height_index_mat[i];
                sig_wave_height_index_location[i] = sig_wave_height_index;
                energy_period_index_mat[i] = (ssc_number_t)(wave_power_matrix.at(0, size_t(energy_period_index)));
                energy_period_index_mat_interp[i * 3] = energy_period_index_mat[i];
                energy_period_index_mat_interp[i * 3 + 1] = energy_period_index_mat[i];
                energy_period_index_mat_interp[i * 3 + 2] = energy_period_index_mat[i];
                energy_period_index_location[i] = energy_period_index;
                wave_power_index_mat[i] = (ssc_number_t)(wave_power_matrix.at(size_t(sig_wave_height_index), size_t(energy_period_index)));
                annual_energy += energy_hourly[i];
                //device_average_power += energy_hourly[i] / 8760;
                device_average_power += energy_hourly[i] / number_hours;
                

            }
            p_annual_energy_dist[0] = 0; //set top left corner of matrix to 0
            p_annual_energy_dist_time[0] = 0;
            
            assign("numberRecords", number_records);
            assign("numberHours", number_hours);
        }


        

		/*
		//Throw exception if default header row (of power curve and resource) does not match user input header column:
		std::vector<double> _check_header{ 0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5,	11.5, 12.5,	13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5 };
		if (_check_header != wave_resource_matrix[0])
			throw compute_module::exec_error("mhk_wave", "Time period bins of resource matrix don't match. Reset bins to default");
		if (_check_header != wave_power_matrix[0])
			throw compute_module::exec_error("mhk_wave", "Time period bins of wave power matrix don't match. Reset bins to default"); */


		//Factoring in losses in total annual energy production:
		//Factoring in losses in total annual energy production:
		annual_energy *= (1 - (total_loss / 100));
		// leave device average power without losses
		annual_energy *= number_devices;

        //TEST cost metrics in tidal page rather than cost page
        double device_cost = as_double("device_costs_total");
        double bos_cost = as_double("balance_of_system_cost_total");
        double financial_cost = as_double("financial_cost_total");
        double om_cost = as_double("total_operating_cost");
        double fcr = as_double("fixed_charge_rate");
        double total_capital_cost_kwh = fcr*(device_cost + bos_cost + financial_cost) / annual_energy;
        double total_device_cost_kwh = fcr*device_cost / annual_energy;
        double total_bos_cost_kwh = fcr*bos_cost / annual_energy;
        double total_financial_cost_kwh = fcr*financial_cost / annual_energy;
        double total_om_cost_kwh = om_cost / annual_energy;
        double total_capital_cost_lcoe = (fcr * (device_cost + bos_cost + financial_cost)) / (fcr * (device_cost + bos_cost + financial_cost) + om_cost) * 100;
        double total_device_cost_lcoe = (fcr * device_cost) / (fcr * (device_cost + bos_cost + financial_cost) + om_cost) * 100;
        double total_bos_cost_lcoe = (fcr * bos_cost) / (fcr * (device_cost + bos_cost + financial_cost) + om_cost) * 100;
        double total_financial_cost_lcoe = (fcr * financial_cost) / (fcr * (device_cost + bos_cost + financial_cost) + om_cost) * 100;
        double total_om_cost_lcoe = (om_cost) / (fcr * (device_cost + bos_cost + financial_cost) + om_cost) * 100;
        assign("total_capital_cost_kwh", var_data((ssc_number_t)total_capital_cost_kwh));
        assign("total_device_cost_kwh", var_data((ssc_number_t)total_device_cost_kwh));
        assign("total_bos_cost_kwh", var_data((ssc_number_t)total_bos_cost_kwh));
        assign("total_financial_cost_kwh", var_data((ssc_number_t)total_financial_cost_kwh));
        assign("total_om_cost_kwh", var_data((ssc_number_t)total_om_cost_kwh));
        assign("total_capital_cost_lcoe", var_data((ssc_number_t)total_capital_cost_lcoe));
        assign("total_device_cost_lcoe", var_data((ssc_number_t)total_device_cost_lcoe));
        assign("total_bos_cost_lcoe", var_data((ssc_number_t)total_bos_cost_lcoe));
        assign("total_financial_cost_lcoe", var_data((ssc_number_t)total_financial_cost_lcoe));
        assign("total_om_cost_lcoe", var_data((ssc_number_t)total_om_cost_lcoe));

		//Calculating capacity factor:
		capacity_factor = annual_energy / (device_rated_capacity * number_devices * 8760);
        //capacity_factor = annual_energy / (device_rated_capacity * number_devices * number_hours);
		
		//Assigning values to outputs:
		assign("annual_energy", var_data((ssc_number_t)annual_energy));
		assign("average_power", var_data((ssc_number_t)device_average_power));
		//assign("system_capacity", var_data((ssc_number_t)system_capacity));
		assign("capacity_factor", var_data((ssc_number_t)capacity_factor * 100));
		assign("device_average_power", var_data((ssc_number_t)device_average_power));
        


	}
};

DEFINE_MODULE_ENTRY(mhk_wave, "MHK Wave power calculation model using power distribution.", 3);
