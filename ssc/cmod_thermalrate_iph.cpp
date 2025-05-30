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

#include "core.h"
#include <algorithm>
#include <sstream>


  
static var_info vtab_thermal_rate_iph[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "en_thermal_rates",           "Optionally enable/disable thermal_rate",                   "years",  "",                      "Thermal Rate",             "",                         "INTEGER,MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "Lifetime",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "Lifetime", "*", "INTEGER,MIN=0,MAX=1", "" },

	// First year or lifetime hourly or subhourly
	// load and gen expected to be > 0
	{ SSC_INPUT, SSC_ARRAY, "gen_heat", "Thermal power generated", "kWt", "", "Thermal Rate", "*", "", "" },
	 
	// input from user as MMBtu/hr and output as MMBtu/hr
	{ SSC_INOUT, SSC_ARRAY, "thermal_load_heat_btu", "Thermal load (year 1)", "MMBtu/hr", "", "Thermal Rate", "", "", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Lifetime", "*", "MIN=-99", "" },

	{ SSC_INPUT, SSC_ARRAY, "thermal_degradation", "Annual energy degradation", "%", "", "Thermal Rate", "?=0", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "thermal_load_escalation", "Annual load escalation", "%/year", "", "Thermal Rate", "?=0", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "thermal_rate_escalation",          "Annual thermal rate escalation",  "%/year", "",                      "Thermal Rate",             "?=0",                       "",                              "" },

    { SSC_INPUT, SSC_NUMBER, "thermal_conversion_efficiency",       "Heat conversion efficiency (buy)", "%",     "",                      "Thermal Rate",             "?=100",                     "",                              "" },


	{ SSC_INPUT, SSC_NUMBER, "thermal_buy_rate_option",             "Thermal buy rate option",   "0-2",          "0=flat,1=timestep,2=monthly", "Thermal Rate",       "?=0",                       "INTEGER,MIN=0,MAX=2",           "" },
    { SSC_INPUT, SSC_NUMBER, "thermal_buy_rate_flat_heat_btu",      "Thermal buy rate flat",     "$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },
    { SSC_INPUT, SSC_ARRAY,  "thermal_timestep_buy_rate_heat_btu",  "Thermal buy rate",          "$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },
    { SSC_INPUT, SSC_ARRAY,  "thermal_monthly_buy_rate_heat_btu",   "Monthly thermal buy rate",  "$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },

    { SSC_INPUT, SSC_NUMBER, "thermal_sell_rate_option",            "Thermal sell rate option",  "0-2", "0=flat,1=timestep,2=monthly",    "Thermal Rate",             "?=0",                       "INTEGER,MIN=0,MAX=2",           "" },
	{ SSC_INPUT, SSC_NUMBER, "thermal_sell_rate_flat_heat_btu",     "Thermal sell rate flat",    "$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },
    { SSC_INPUT, SSC_ARRAY,  "thermal_timestep_sell_rate_heat_btu", "Thermal sell rate timestep","$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },
    { SSC_INPUT, SSC_ARRAY,  "thermal_monthly_sell_rate_heat_btu",  "Thermal sell rate monthly", "$/MMBtu",      "",                      "Thermal Rate",             "?=0",                       "",                              "" },


	{ SSC_OUTPUT, SSC_ARRAY, "annual_thermal_value", "Thermal value", "$", "", "Annual", "*", "", "" },
    { SSC_OUTPUT, SSC_ARRAY, "thermal_revenue_with_system", "Thermal revenue with system", "$", "", "Time Series", "*", "", "" },
    { SSC_OUTPUT, SSC_ARRAY, "thermal_revenue_without_system", "Thermal revenue without system", "$", "", "Time Series", "*", "", "" },
    { SSC_OUTPUT, SSC_ARRAY, "thermal_cost_with_system", "Thermal cost with system", "$", "", "Time Series", "*", "", "" },
    { SSC_OUTPUT, SSC_ARRAY, "thermal_cost_without_system", "Thermal cost without system", "$", "", "Time Series", "*", "", "" },
    { SSC_OUTPUT, SSC_NUMBER, "thermal_load_year1", "Thermal load total", "MMBtu/hr", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "thermal_savings_year1", "Thermal savings (year 1)", "$", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "thermal_cost_with_system_year1", "Thermal cost with sytem (year 1)", "$", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "thermal_cost_without_system_year1", "Thermal cost without system (year 1)", "$", "", "", "*", "", "" },

    { SSC_OUTPUT, SSC_ARRAY, "year1_monthly_load_heat", "Thermal load", "kWht/mo", "", "Monthly", "*", "LENGTH=12", "" },



	var_info_invalid };

class tr_month
{
public:
	// period numbers
	// net energy use per month
	ssc_number_t thermal_net;
	ssc_number_t thermal_load;
	ssc_number_t thermal_gen;
	// hours per period per month
	int hours_per_month;
	ssc_number_t thermal_peak;
	int thermal_peak_hour;
	ssc_number_t thermal_buy;
	ssc_number_t thermal_sell;
};

class cm_thermalrate_iph : public compute_module
{
private:
	size_t m_num_rec_yearly;
	std::vector<tr_month> m_month;


public:
	cm_thermalrate_iph()
	{
		add_var_info( vtab_thermal_rate_iph );
	}

	void exec( )
	{
		// if not assigned, we assume thermal rates are enabled
		if (is_assigned("en_thermal_rates")) {
			if (!as_boolean("en_thermal_rates")) {
				remove_var_info(vtab_thermal_rate_iph);
				return;
			}
		}

        // Convert Generation to MMBtu
        const double MMBTU_TO_KWh = 293.07107; // 1 MMBtu = 293.07107 kWh (also 1 MMBtu/hr == 293.07107 kW)
        std::vector<double> gen_heat_kW = as_vector_double("gen_heat"); // [kW]
        std::vector<double> gen_heat_MMBtu_per_hr;
        for (double gen_kW : gen_heat_kW)
        {
            gen_heat_MMBtu_per_hr.push_back(gen_kW / MMBTU_TO_KWh);
        }


		ssc_number_t *parr = 0;
		size_t count, i, j; 

		size_t nyears = (size_t)as_integer("analysis_period");
		double inflation_rate = as_double("inflation_rate")*0.01;

		// compute annual system output degradation multipliers
		std::vector<ssc_number_t> sys_scale(nyears);

		// degradation
		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		// lifetime degradation applied in technology compute modules
		if (as_integer("system_use_lifetime_output") == 1)
		{
			for (i = 0; i<nyears; i++)
				sys_scale[i] = 1.0;
		}
		else
		{
			parr = as_array("thermal_degradation", &count);
			if (count == 1)
			{
				for (i = 0; i<nyears; i++)
					sys_scale[i] = (ssc_number_t)pow((double)(1 - parr[0] * 0.01), (double)i);
			}
			else
			{
				for (i = 0; i<nyears && i<count; i++)
					sys_scale[i] = (ssc_number_t)(1.0 - parr[i] * 0.01);
			}
		}



		// compute load (thermaltric demand) annual escalation multipliers
		std::vector<ssc_number_t> load_scale(nyears);
		parr = as_array("thermal_load_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)pow( (double)(1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				load_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}

		// compute utility rate out-years escalation multipliers
		std::vector<ssc_number_t> rate_scale(nyears);
		parr = as_array("thermal_rate_escalation", &count);
		if (count == 1)
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)pow( (double)(inflation_rate+1+parr[0]*0.01), (double)i );
		}
		else
		{
			for (i=0;i<nyears;i++)
				rate_scale[i] = (ssc_number_t)(1 + parr[i]*0.01);
		}
 
		// set up buy and sell rates



		ssc_number_t *pload = NULL, *pgen;
		ssc_number_t *pbuyrate = NULL, *psellrate = NULL;
		size_t nrec_load = 0, nrec_gen = 0, step_per_hour_gen=1, step_per_hour_load=1;
		bool bload=false;
		//pgen = as_array("fuelcell_power_thermal", &nrec_gen);
        pgen = &gen_heat_MMBtu_per_hr[0];
        nrec_gen = gen_heat_MMBtu_per_hr.size();

		// for lifetime analysis
		size_t nrec_gen_per_year = nrec_gen;
		if (as_integer("system_use_lifetime_output") == 1)
			nrec_gen_per_year = nrec_gen / nyears;
		step_per_hour_gen = nrec_gen_per_year / 8760;
		if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
			throw exec_error("thermalrate_iph", util::format("invalid number of thermal records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;
		m_num_rec_yearly = nrec_gen_per_year;


        // prepare timestep arrays for load and grid values
        std::vector<ssc_number_t>
            e_sys_cy(m_num_rec_yearly), p_sys_cy(m_num_rec_yearly),
            p_load(m_num_rec_yearly), // to handle no load, or num load != num gen resets above assignment
            p_buyrate(m_num_rec_yearly),
            p_sellrate(m_num_rec_yearly),
            e_grid_cy(m_num_rec_yearly), p_grid_cy(m_num_rec_yearly),
            e_load_cy(m_num_rec_yearly), p_load_cy(m_num_rec_yearly); // current year load (accounts for escal)



		if (is_assigned("thermal_load_heat_btu"))
		{ // hourly or sub hourly loads for single year

            // Convert thermal load units
			bload = true;
            pload = as_array("thermal_load_heat_btu", &nrec_load);//[MMBtu/hr]

			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("thermalrate_iph", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
			if ((nrec_load != m_num_rec_yearly) && (nrec_load != 8760))
				throw exec_error("thermalrate_iph", util::format("number of load records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nrec_load, (int)m_num_rec_yearly));
		}
//		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;






		size_t idx = 0;

        // Get conversion efficiency
        double eff_buy_frac = as_double("thermal_conversion_efficiency") / 100.0;   // Bought heat conversion efficiency (converted to fraction)
        if (eff_buy_frac <= 0)
        {
            throw exec_error("thermalrate_iph", "Conversion efficiency must be greater than 0");
        }

        // Timestep Buy Rate
		if (as_integer("thermal_buy_rate_option") == 1)
		{
			size_t nbuyrate,step_per_hour_br;
			ssc_number_t br;
			pbuyrate = as_array("thermal_timestep_buy_rate_heat_btu", &nbuyrate);
			step_per_hour_br = nbuyrate / 8760;
			if (step_per_hour_br < 1 || step_per_hour_br > 60 || step_per_hour_br * 8760 != nbuyrate)
				throw exec_error("thermalrate_iph", util::format("invalid number of buy rate records (%d): must be an integer multiple of 8760", (int)nbuyrate));
			if ((nbuyrate != m_num_rec_yearly) && (nbuyrate != 8760))
				throw exec_error("thermalrate_iph", util::format("number of buy rate  records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nbuyrate, (int)m_num_rec_yearly));
			for (i = 0; i < 8760; i++)
			{
				for (size_t ii = 0; ii < step_per_hour_gen; ii++)
				{
					size_t ndx = i * step_per_hour_gen + ii;
					br = ((idx < nbuyrate) ? pbuyrate[idx] : 0);
					p_buyrate[ndx] = br / eff_buy_frac; // account for heat conversion efficiency
					if (step_per_hour_gen == step_per_hour_br)
						idx++;
					else if (ii == (step_per_hour_gen - 1))
						idx++;
				}
			}
		}
        // Monthly Buy Rate
        else if (as_integer("thermal_buy_rate_option") == 2)
        {
            std::vector<double> br_monthly = as_vector_double("thermal_monthly_buy_rate_heat_btu");
            if (br_monthly.size() != 12)
            {
                throw exec_error("thermalrate_iph", util::format("invalid number of monthly buy rate records (%d): must be equal to 12", (int)br_monthly.size()));
            }
            // Assign buy rate for every hour in each month
            int hr_count = 0;
            for (int month = 1; month <= 12; month++)
            {
                int hr_in_current_month = util::hours_in_month(month);
                for (int hr_in_mnth = 0; hr_in_mnth < hr_in_current_month; hr_in_mnth++)
                {
                    p_buyrate[hr_count] = br_monthly[month - 1] / eff_buy_frac; // account for heat conversion efficiency
                    hr_count++;
                }
            }
        }
		else // flat rate
		{
			ssc_number_t br = as_number("thermal_buy_rate_flat_heat_btu");
			for (i = 0; i < m_num_rec_yearly; i++)
				p_buyrate[i] = br / eff_buy_frac;   // account for heat conversion efficiency
		}

        // Time step input
		if (as_integer("thermal_sell_rate_option") == 1)
		{
			size_t nsellrate, step_per_hour_br;
			ssc_number_t br;
			psellrate = as_array("thermal_timestep_sell_rate_heat_btu", &nsellrate);
			step_per_hour_br = nsellrate / 8760;
			if (step_per_hour_br < 1 || step_per_hour_br > 60 || step_per_hour_br * 8760 != nsellrate)
				throw exec_error("thermalrate_iph", util::format("invalid number of sell rate records (%d): must be an integer multiple of 8760", (int)nsellrate));
			if ((nsellrate != m_num_rec_yearly) && (nsellrate != 8760))
				throw exec_error("thermalrate_iph", util::format("number of sell rate  records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nsellrate, (int)m_num_rec_yearly));
			for (i = 0; i < 8760; i++)
			{
				for (size_t ii = 0; ii < step_per_hour_gen; ii++)
				{
					size_t ndx = i * step_per_hour_gen + ii;
					br = ((idx < nsellrate) ? psellrate[idx] : 0);
					p_sellrate[ndx] = br;
					if (step_per_hour_gen == step_per_hour_br)
						idx++;
					else if (ii == (step_per_hour_gen - 1))
						idx++;
				}
			}
		}
        // Monthly input
        else if (as_integer("thermal_sell_rate_option") == 2)
        {
            std::vector<double> sr_monthly = as_vector_double("thermal_monthly_sell_rate_heat_btu");
            if (sr_monthly.size() != 12)
            {
                throw exec_error("thermalrate_iph", util::format("invalid number of monthly sell rate records (%d): must be equal to 12", (int)sr_monthly.size()));
            }
            // Assign sell rate for every hour in each month
            int hr_count = 0;
            for (int month = 1; month <= 12; month++)
            {
                int hr_in_current_month = util::hours_in_month(month);
                for (int hr_in_mnth = 0; hr_in_mnth < hr_in_current_month; hr_in_mnth++)
                {
                    p_sellrate[hr_count] = sr_monthly[month - 1];
                    hr_count++;
                }
            }
        }
		else // flat rate
		{
			ssc_number_t br = as_number("thermal_sell_rate_flat_heat_btu");
			for (i = 0; i < m_num_rec_yearly; i++)
				p_sellrate[i] = br;
		}


	
		// assign timestep values for utility rate calculations
		ssc_number_t ts_load = 0;
		ssc_number_t year1_thermal_load = 0;

		//load - fill out to number of generation records per year
		// handle cases 
		// 1. if no load 
		// 2. if load has 8760 and gen has more records
		// 3. if number records same for load and gen
		idx = 0;
		for (i = 0; i < 8760; i++)
		{
			for (size_t ii = 0; ii < step_per_hour_gen; ii++)
			{
				size_t ndx = i*step_per_hour_gen + ii;
				ts_load = (bload ? ((idx < nrec_load) ? pload[idx] : 0) : 0);
				year1_thermal_load += ts_load;
				// sign correction for utility rate calculations
				p_load[ndx] = -ts_load;
				if (step_per_hour_gen == step_per_hour_load)
					idx++;
				else if (ii == (step_per_hour_gen - 1))
					idx++;
			}
		}

		assign("thermal_load_year1", year1_thermal_load* ts_hour_gen);

		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(m_num_rec_yearly), revenue_wo_sys(m_num_rec_yearly),
			payment(m_num_rec_yearly), income(m_num_rec_yearly), 
			thermal_charge_w_sys(m_num_rec_yearly), thermal_charge_wo_sys(m_num_rec_yearly),
			load(m_num_rec_yearly), salespurchases(m_num_rec_yearly);
        std::vector<ssc_number_t>
            monthly_salespurchases(12),
            monthly_load_heat_btu(12),
            monthly_load_heat(12);

		/* allocate outputs */		
		ssc_number_t *annual_net_revenue = allocate("annual_thermal_value", nyears+1);
		ssc_number_t *annual_thermal_load = allocate("annual_thermal_load", nyears+1);
		ssc_number_t *thermal_net = allocate("scaled_annual_thermal_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("annual_thermal_revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("annual_thermal_revenue_without_system", nyears+1);
		ssc_number_t *annual_thermal_cost_w_sys = allocate("thermal_cost_with_system", nyears+1);
		ssc_number_t *annual_thermal_cost_wo_sys = allocate("thermal_cost_without_system", nyears+1);

        size_t steps_per_hour = m_num_rec_yearly / 8760;
        int c = 0;
        for (int m = 0; m < 12; m++)
        {
            monthly_load_heat_btu[m] = 0;
            monthly_load_heat[m] = 0;
            for (int d = 0; d < (int)util::nday[m]; d++)
            {
                for (int h = 0; h < 24; h++)
                {
                    for (int s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
                    {
                        monthly_load_heat_btu[m] -= p_load[c];  //[MMBtu]
                        monthly_load_heat[m] -= p_load[c] * MMBTU_TO_KWh;  //[kWht]
                        c++;
                    }
                }
            }
        }
        // Assign monthly load
        assign("year1_monthly_load_heat", var_data(&monthly_load_heat[0], 12));

		// lifetime hourly load
		ssc_number_t *lifetime_load = allocate("lifetime_thermal_load", nrec_gen);

		idx = 0;
		for (i=0;i<nyears;i++)
		{
			for (j = 0; j<m_num_rec_yearly; j++)
			{
				e_load_cy[j] = p_load[j] * load_scale[i] * ts_hour_gen;
				p_load_cy[j] = p_load[j] * load_scale[i];


				// update e_sys per year if lifetime output
				if ((as_integer("system_use_lifetime_output") == 1) && ( idx < nrec_gen ))
				{
					e_sys_cy[j] = pgen[idx] * ts_hour_gen;
					p_sys_cy[j] = pgen[idx];
					// until lifetime load fully implemented
					lifetime_load[idx] = -e_load_cy[j]/ts_hour_gen;
					idx++;
				}
				else
				{
					e_sys_cy[j] = pgen[j] * ts_hour_gen;
					p_sys_cy[j] = pgen[j];
				}
				e_sys_cy[j] *= sys_scale[i];
				p_sys_cy[j] *= sys_scale[i];
				// calculate e_grid value (e_sys + e_load)
//				e_sys_cy[j] = e_sys[j] * sys_scale[i];
//				p_sys_cy[j] = p_sys[j] * sys_scale[i];
				// note: load is assumed to have negative sign
				e_grid_cy[j] = e_sys_cy[j] + e_load_cy[j];
				p_grid_cy[j] = p_sys_cy[j] + p_load_cy[j];
			}

			// without system
			tr_calc_timestep(&e_load_cy[0], &p_load_cy[0], &p_buyrate[0], &p_sellrate[0],
				&revenue_wo_sys[0], &payment[0], &income[0], &thermal_charge_wo_sys[0], rate_scale[i]);


			if (i==0)
			{
				assign("year1_hourly_charge_without_system", var_data(&thermal_charge_wo_sys[0], m_num_rec_yearly));

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					salespurchases[ii] = revenue_wo_sys[ii];
				}

				int c = 0;

				for (int m=0;m<12;m++)
				{
					monthly_salespurchases[m] = 0;
					for (size_t d=0;d<util::nday[m];d++)
					{
						for(int h=0;h<24;h++)
						{
							for (size_t s = 0; s < step_per_hour_gen; s++)
							{
								monthly_salespurchases[m] += salespurchases[c];
								c++;
							}
						}
					}
				}
				assign("thermal_revenue_without_system", var_data(&revenue_wo_sys[0], (int)m_num_rec_yearly));
				assign("year1_monthly_thermal_bill_wo_sys", var_data(&monthly_salespurchases[0], 12));
			}
			
// with system

		tr_calc_timestep(&e_grid_cy[0], &p_grid_cy[0], &p_buyrate[0], &p_sellrate[0], &revenue_w_sys[0], &payment[0], &income[0], &thermal_charge_w_sys[0], rate_scale[i]);
			
			if (i == 0)
			{
				assign("year1_hourly_charge_with_system", var_data(&thermal_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("thermal_revenue_with_system", var_data(&revenue_w_sys[0], (int)m_num_rec_yearly));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough thermal to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(m_num_rec_yearly), tdemand(m_num_rec_yearly), pdemand(m_num_rec_yearly), e_sys_to_grid(m_num_rec_yearly), e_sys_to_load(m_num_rec_yearly), p_sys_to_load(m_num_rec_yearly);
				for (j = 0; j<m_num_rec_yearly; j++)
				{
					output[j] = e_sys_cy[j];
					tdemand[j] = e_grid_cy[j] < 0.0 ? -e_grid_cy[j] : (ssc_number_t)0.0;
					pdemand[j] = p_grid_cy[j] < 0.0 ? -p_grid_cy[j] : (ssc_number_t)0.0;

					ssc_number_t sys_e_net = output[j] + e_load_cy[j];// loads are assumed negative
					e_sys_to_grid[j] = sys_e_net > 0 ? sys_e_net : (ssc_number_t)0.0;
					e_sys_to_load[j] = sys_e_net > 0 ? -e_load_cy[j] : output[j];

//					ssc_number_t sys_p_net = output[j] + p_load[j];// loads are assumed negative
//					p_sys_to_load[j] = sys_p_net > 0 ? -p_load[j] : output[j];
					ssc_number_t sys_p_net = output[j] + p_load_cy[j];// loads are assumed negative
					p_sys_to_load[j] = sys_p_net > 0 ? -p_load_cy[j] : output[j];
				}

				assign("year1_hourly_system_output", var_data(&output[0], (int)m_num_rec_yearly));
				assign("year1_hourly_t_demand", var_data(&tdemand[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], (int)m_num_rec_yearly));

				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], (int)m_num_rec_yearly));


			}
			
			// determine net-revenue benefit due to thermal for year 'i'
			
			annual_net_revenue[i+1] = 0.0;
			annual_thermal_load[i + 1] = 0.0;
			thermal_net[i + 1] = 0.0;
			annual_revenue_w_sys[i + 1] = 0.0;
			annual_revenue_wo_sys[i + 1] = 0.0;

			for (j = 0; j<m_num_rec_yearly; j++) 
			{
				thermal_net[i + 1] +=  e_sys_cy[j];
//				annual_net_revenue[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				annual_thermal_load[i + 1] += -e_load_cy[j];
				annual_revenue_w_sys[i + 1] += revenue_w_sys[j];
				annual_revenue_wo_sys[i + 1] += revenue_wo_sys[j];
			}

			annual_thermal_cost_w_sys[i + 1] = -annual_revenue_w_sys[i+1];
			annual_thermal_cost_wo_sys[i + 1] = -annual_revenue_wo_sys[i+1];
			annual_net_revenue[i + 1] = annual_thermal_cost_wo_sys[i + 1] - annual_thermal_cost_w_sys[i + 1];

		}

		assign("thermal_cost_with_system_year1", annual_thermal_cost_w_sys[1]);
		assign("thermal_cost_without_system_year1", annual_thermal_cost_wo_sys[1]);
		assign("thermal_savings_year1", annual_thermal_cost_wo_sys[1] - annual_thermal_cost_w_sys[1]);
	}

	void tr_calc_timestep(ssc_number_t *e_in, ssc_number_t *p_in, ssc_number_t *br_in, ssc_number_t *sr_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *thermal_charge,
		ssc_number_t rate_esc, bool = true, bool = true, bool = false)

	{
		int i;
		for (i = 0; i<(int)m_num_rec_yearly; i++)
			revenue[i] = payment[i] = income[i] = thermal_charge[i] = 0.0;

		 

		size_t steps_per_hour = m_num_rec_yearly / 8760;


		// calculate the monthly net energy and monthly hours
		int m, d, h, s;
		size_t c = 0;
		for (m = 0; m < (int)m_month.size(); m++)
		{
			m_month[m].thermal_net = 0;
			m_month[m].hours_per_month = 0;
			m_month[m].thermal_peak = 0;
			m_month[m].thermal_peak_hour = 0;
			for (d = 0; d < (int)util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && (int)c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						m_month[m].thermal_net += e_in[c]; // -load and +gen
						// hours per period per month
						m_month[m].hours_per_month++;
						// peak
						if (p_in[c] < 0 && p_in[c] < -m_month[m].thermal_peak)
						{
							m_month[m].thermal_peak = -p_in[c];
							m_month[m].thermal_peak_hour = (int)c;
						}
						c++;
					}
				}
			}
		}


// main loop
		c = 0; // hourly count
		// process one timestep at a time
		for (m = 0; m < 12; m++)
		{
			for (d = 0; d<(int)util::nday[m]; d++)
			{
				//daily_net_energy = 0;
				for (h = 0; h<24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && (int)c < (int)m_num_rec_yearly; s++)
					{

						if (e_in[c] >= 0.0)
						{ // calculate income or credit

							// cumulative energy used to determine tier for credit of entire surplus amount
							ssc_number_t credit_amt = 0;
							ssc_number_t thermal_surplus = e_in[c];
							credit_amt = thermal_surplus * sr_in[c] * rate_esc;
							// accumulate monthly charge and therms
							income[c] = (ssc_number_t)credit_amt;
						}
						else
						{ // calculate payment or charge
							ssc_number_t charge_amt = 0;
							ssc_number_t thermal_deficit = -e_in[c];
							charge_amt = thermal_deficit * br_in[c] * rate_esc;
							// accumulate monthly charge and therms
							payment[c] = (ssc_number_t)charge_amt;
							//monthly_charges[m] += (ssc_number_t)charge_amt;

						}
						revenue[c] = income[c] - payment[c];

						c++;
					} // steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill 

		} // end of month m (m loop)


	}



};

DEFINE_MODULE_ENTRY( thermalrate_iph, "Thermal flat rate structure net revenue calculator", 1 );


