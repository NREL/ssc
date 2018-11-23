/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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
#include <algorithm>
#include <sstream>


  
static var_info vtab_thermal_rate[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "en_electricity_rates",           "Optionally enable/disable electricity_rate",                   "years",  "",                      "",             "",                         "INTEGER,MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",           "Number of years in analysis",                   "years",  "",                      "",             "*",                         "INTEGER,POSITIVE",              "" },

	{ SSC_INPUT, SSC_NUMBER, "system_use_lifetime_output", "Lifetime hourly system outputs", "0/1", "0=hourly first year,1=hourly lifetime", "", "*", "INTEGER,MIN=0,MAX=1", "" },

	{ SSC_INPUT, SSC_NUMBER, "TOU_demand_single_peak", "Use single monthly peak for TOU demand charge", "0/1", "0=use TOU peak,1=use flat peak", "", "?=0", "INTEGER,MIN=0,MAX=1", "" },
	
	// First year or lifetime hourly or subhourly
	// load and gen expected to be > 0
	// grid positive if system generation > load, negative otherwise
	{ SSC_INPUT, SSC_ARRAY, "gen", "System power generated", "kW", "", "Time Series", "*", "", "" },
	 
	// input from user as kW and output as kW
	{ SSC_INOUT, SSC_ARRAY, "load", "Electricity load (year 1)", "kW", "", "Time Series", "", "", "" },
	//  output as kWh - same as load (kW) for hourly simulations
	{ SSC_OUTPUT, SSC_ARRAY, "bill_load", "Bill load (year 1)", "kWh", "", "Time Series", "*", "", "" },

	{ SSC_INPUT, SSC_NUMBER, "inflation_rate", "Inflation rate", "%", "", "Financials", "*", "MIN=-99", "" },

	{ SSC_INPUT, SSC_ARRAY, "degradation", "Annual energy degradation", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "load_escalation", "Annual load escalation", "%/year", "", "", "?=0", "", "" },
	{ SSC_INPUT,        SSC_ARRAY,      "rate_escalation",          "Annual electricity rate escalation",  "%/year", "",                      "",             "?=0",                       "",                              "" },

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

class cm_thermalrate : public compute_module
{
private:
	size_t m_num_rec_yearly;
	std::vector<tr_month> m_month;


public:
	cm_thermalrate()
	{
		add_var_info( vtab_thermal_rate );
	}

	void exec( ) throw( general_error )
	{
		// if not assigned, we assume electricity rates are enabled
		if (is_assigned("en_electricity_rates")) {
			if (!as_boolean("en_electricity_rates")) {
				remove_var_info(vtab_thermal_rate);
				return;
			}
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
			parr = as_array("degradation", &count);
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



		// compute load (electric demand) annual escalation multipliers
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
 
		/* Update all e_sys and e_load values based on new inputs
		grid = gen -load where gen = sys + batt
		1. scale load and system value to hourly values as necessary
		2. use (kWh) e_sys[i] = sum((grid+load) * timestep ) over the hour for each hour i
		3. use (kW)  p_sys[i] = max( grid+load) over the hour for each hour i
		3. use (kWh) e_load[i] = sum(load * timestep ) over the hour for each hour i
		4. use (kW)  p_load[i] = max(load) over the hour for each hour i
		5. After above assignment, proceed as before with same outputs
		*/
		ssc_number_t *pload = NULL, *pgen;
		size_t nrec_load = 0, nrec_gen = 0, step_per_hour_gen=1, step_per_hour_load=1;
		bool bload=false;
		pgen = as_array("thermal_gen", &nrec_gen);
		// for lifetime analysis
		size_t nrec_gen_per_year = nrec_gen;
		if (as_integer("system_use_lifetime_output") == 1)
			nrec_gen_per_year = nrec_gen / nyears;
		step_per_hour_gen = nrec_gen_per_year / 8760;
		if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
			throw exec_error("thermalrate", util::format("invalid number of thermal records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year));
		ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;
		m_num_rec_yearly = nrec_gen_per_year;

		if (is_assigned("thermal_load"))
		{ // hourly or sub hourly loads for single year
			bload = true;
			pload = as_array("thermal_load", &nrec_load);
			step_per_hour_load = nrec_load / 8760;
			if (step_per_hour_load < 1 || step_per_hour_load > 60 || step_per_hour_load * 8760 != nrec_load)
				throw exec_error("thermalrate", util::format("invalid number of load records (%d): must be an integer multiple of 8760", (int)nrec_load));
			if ((nrec_load != m_num_rec_yearly) && (nrec_load != 8760))
				throw exec_error("thermalrate", util::format("number of load records (%d) must be equal to number of gen records (%d) or 8760 for each year", (int)nrec_load, (int)m_num_rec_yearly));
		}
//		ssc_number_t ts_hour_load = 1.0f / step_per_hour_load;

		// prepare timestep arrays for load and grid values
		std::vector<ssc_number_t> 
			e_sys_cy(m_num_rec_yearly), p_sys_cy(m_num_rec_yearly),
			p_load(m_num_rec_yearly), // to handle no load, or num load != num gen
			e_grid_cy(m_num_rec_yearly), p_grid_cy(m_num_rec_yearly),
			e_load_cy(m_num_rec_yearly), p_load_cy(m_num_rec_yearly); // current year load (accounts for escal)



		// assign timestep values for utility rate calculations
		size_t idx = 0;
		ssc_number_t ts_load = 0;
		ssc_number_t year1_elec_load = 0;

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
				year1_elec_load += ts_load;
				// sign correction for utility rate calculations
				p_load[ndx] = -ts_load;
				if (step_per_hour_gen == step_per_hour_load)
					idx++;
				else if (ii == (step_per_hour_gen - 1))
					idx++;
			}
		}

		assign("year1_thermal_load", year1_elec_load* ts_hour_gen);

		
		/* allocate intermediate data arrays */
		std::vector<ssc_number_t> revenue_w_sys(m_num_rec_yearly), revenue_wo_sys(m_num_rec_yearly),
			payment(m_num_rec_yearly), income(m_num_rec_yearly), 
			demand_charge_w_sys(m_num_rec_yearly), energy_charge_w_sys(m_num_rec_yearly), energy_charge_gross_w_sys(m_num_rec_yearly),
			demand_charge_wo_sys(m_num_rec_yearly), energy_charge_wo_sys(m_num_rec_yearly),
			ec_tou_sched(m_num_rec_yearly), dc_tou_sched(m_num_rec_yearly), load(m_num_rec_yearly), dc_hourly_peak(m_num_rec_yearly),
			e_tofromgrid(m_num_rec_yearly), p_tofromgrid(m_num_rec_yearly), salespurchases(m_num_rec_yearly);
		std::vector<ssc_number_t> monthly_revenue_w_sys(12), monthly_revenue_wo_sys(12),
			monthly_fixed_charges(12), monthly_minimum_charges(12),
			monthly_dc_fixed(12), monthly_dc_tou(12),
			monthly_ec_charges(12),
			monthly_ec_charges_gross(12),
			monthly_excess_dollars_applied(12),
			monthly_excess_dollars_earned(12),
			monthly_excess_kwhs_applied(12),
			monthly_excess_kwhs_earned(12),
			monthly_ec_rates(12),
			monthly_salespurchases(12),
			monthly_load(12), monthly_system_generation(12), monthly_elec_to_grid(12),
			monthly_elec_needed_from_grid(12),
			monthly_cumulative_excess_energy(12), monthly_cumulative_excess_dollars(12), monthly_bill(12), monthly_peak(12), monthly_test(12);

		/* allocate outputs */		
		ssc_number_t *annual_net_revenue = allocate("annual_thermal_value", nyears+1);
		ssc_number_t *annual_electric_load = allocate("annual_thermal_load", nyears+1);
		ssc_number_t *thermal_net = allocate("scaled_annual_thermal_energy", nyears+1);
		ssc_number_t *annual_revenue_w_sys = allocate("thermal_revenue_with_system", nyears+1);
		ssc_number_t *annual_revenue_wo_sys = allocate("thermal_revenue_without_system", nyears+1);
		ssc_number_t *annual_elec_cost_w_sys = allocate("thermal_cost_with_system", nyears+1);
		ssc_number_t *annual_elec_cost_wo_sys = allocate("thermal_cost_without_system", nyears+1);

		// matrices
		ssc_number_t *utility_bill_w_sys_ym = allocate("thermal_bill_w_sys_ym", nyears + 1, 12);
		ssc_number_t *utility_bill_wo_sys_ym = allocate("thermal_bill_wo_sys_ym", nyears + 1, 12);


		// annual sums
		ssc_number_t *utility_bill_w_sys = allocate("utility_bill_w_sys", nyears + 1);
		ssc_number_t *utility_bill_wo_sys = allocate("utility_bill_wo_sys", nyears + 1);

		// Enphase outputs requested - see emails 2/12/16- first year system to grid and from grid
		ssc_number_t *year1_hourly_thermal_togrid = allocate("year1_hourly_e_togrid", m_num_rec_yearly);
		ssc_number_t *year1_hourly_thermal_fromgrid = allocate("year1_hourly_e_fromgrid", m_num_rec_yearly);






		// lifetime hourly load
		ssc_number_t *lifetime_load = allocate("lifetime_thermal_load", nrec_gen);


		idx = 0;
		for (i=0;i<nyears;i++)
		{
			for (j = 0; j<m_num_rec_yearly; j++)
			{
				/* for future implementation for lifetime loads
				// update e_load and p_load per year if lifetime output
				// lifetime load values? sell values
				if ((as_integer("system_use_lifetime_output") == 1) && (idx < nrec_load))
				{
					e_load[j] = p_load[j] = 0.0;
					for (size_t ii = 0; (ii < step_per_hour_load) && (idx < nrec_load); ii++)
					{
						ts_load = (bload ? pload[idx] : 0);
						e_load[i] += ts_load * ts_hour_load;
						p_load[i] = ((ts_load > p_load[i]) ? ts_load : p_load[i]);
						idx++;
					}
					lifetime_hourly_load[i*8760 + j] = e_load[i];
					// sign correction for utility rate calculations
					e_load[i] = -e_load[i];
					p_load[i] = -p_load[i];
				}
				*/
				// apply load escalation appropriate for current year
//				e_load_cy[j] = e_load[j] * load_scale[i];
//				p_load_cy[j] = p_load[j] * load_scale[i];
				e_load_cy[j] = p_load[j] * load_scale[i] * ts_hour_gen;
				p_load_cy[j] = p_load[j] * load_scale[i];


				// update e_sys per year if lifetime output
				if ((as_integer("system_use_lifetime_output") == 1) && ( idx < nrec_gen ))
				{
//					e_sys[j] = p_sys[j] = 0.0;
//					ts_power = (idx < nrec_gen) ? pgen[idx] : 0;
//					e_sys[j] = ts_power * ts_hour_gen;
//					p_sys[j] = ((ts_power > p_sys[j]) ? ts_power : p_sys[j]);
					e_sys_cy[j] = pgen[idx] * ts_hour_gen;
					p_sys_cy[j] = pgen[idx];
					// until lifetime load fully implemented
					lifetime_load[idx] = -e_load_cy[j];
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


			tr_calc_timestep(&e_load_cy[0], &p_load_cy[0],
				&revenue_wo_sys[0], &payment[0], &income[0], &demand_charge_wo_sys[0], rate_scale[i]);


			if (i==0)
			{
				assign("year1_hourly_dc_without_system", var_data(&demand_charge_wo_sys[0], m_num_rec_yearly));
				assign("year1_hourly_ec_without_system", var_data(&energy_charge_wo_sys[0], m_num_rec_yearly));

				assign("year1_monthly_dc_fixed_without_system", var_data(&monthly_dc_fixed[0], 12));
				assign( "year1_monthly_dc_tou_without_system", var_data(&monthly_dc_tou[0], 12) );
				assign("year1_monthly_ec_charge_without_system", var_data(&monthly_ec_charges[0], 12));

				// sign reversal based on 9/5/13 meeting, reverse again 9/6/13
				for (int ii = 0; ii<(int)m_num_rec_yearly; ii++)
				{
					salespurchases[ii] = revenue_wo_sys[ii];
				}

				int c = 0;

				for (int m=0;m<12;m++)
				{
					monthly_salespurchases[m] = 0;
					for (int d=0;d<util::nday[m];d++)
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
				assign("year1_hourly_salespurchases_without_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_utility_bill_wo_sys", var_data(&monthly_bill[0], 12));
				assign("year1_monthly_fixed_without_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_without_system", var_data(&monthly_minimum_charges[0], 12));
/*
				// peak demand and testing energy use
				for (int ii = 0; ii < 12; ii++)
				{
					monthly_peak[ii] = m_month[ii].thermal_peak;
					monthly_test[ii] = -m_month[ii].thermal_net;
				}
				assign("year1_monthly_peak_wo_system", var_data(&monthly_peak[0], 12));
				assign("year1_monthly_use_wo_system", var_data(&monthly_test[0], 12));
				*/
			}
			
// with system

		tr_calc_timestep(&e_grid_cy[0], &p_grid_cy[0],
						&revenue_w_sys[0], &payment[0], &income[0], 
						&demand_charge_w_sys[0], rate_scale[i]);
			
			if (i == 0)
			{
				assign("year1_hourly_dc_with_system", var_data(&demand_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_ec_with_system", var_data(&energy_charge_w_sys[0], (int)m_num_rec_yearly));
				assign("year1_hourly_dc_peak_per_period", var_data(&dc_hourly_peak[0], (int)m_num_rec_yearly));

				assign("year1_hourly_salespurchases_with_system", var_data(&salespurchases[0], (int)m_num_rec_yearly));
				assign("year1_monthly_load", var_data(&monthly_load[0], 12));
				assign("year1_monthly_system_generation", var_data(&monthly_system_generation[0], 12));
				assign("year1_monthly_electricity_to_grid", var_data(&monthly_elec_to_grid[0], 12));
				assign("year1_monthly_electricity_needed_from_grid", var_data(&monthly_elec_needed_from_grid[0], 12));

				assign("year1_monthly_cumulative_excess_generation", var_data(&monthly_cumulative_excess_energy[0], 12));
				assign("year1_monthly_cumulative_excess_dollars", var_data(&monthly_cumulative_excess_dollars[0], 12));
				assign("year1_monthly_utility_bill_w_sys", var_data(&monthly_bill[0], 12));

				// output and demand per Paul's email 9/10/10
				// positive demand indicates system does not produce enough electricity to meet load
				// zero if the system produces more than the demand
				std::vector<ssc_number_t> output(m_num_rec_yearly), edemand(m_num_rec_yearly), pdemand(m_num_rec_yearly), e_sys_to_grid(m_num_rec_yearly), e_sys_to_load(m_num_rec_yearly), p_sys_to_load(m_num_rec_yearly);
				for (j = 0; j<m_num_rec_yearly; j++)
				{
					output[j] = e_sys_cy[j];
					edemand[j] = e_grid_cy[j] < 0.0 ? -e_grid_cy[j] : (ssc_number_t)0.0;
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
				assign("year1_hourly_e_demand", var_data(&edemand[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_demand", var_data(&pdemand[0], (int)m_num_rec_yearly));

				assign("year1_hourly_system_to_load", var_data(&e_sys_to_load[0], (int)m_num_rec_yearly));
				assign("year1_hourly_p_system_to_load", var_data(&p_sys_to_load[0], (int)m_num_rec_yearly));

				assign("year1_monthly_fixed_with_system", var_data(&monthly_fixed_charges[0], 12));
				assign("year1_monthly_minimum_with_system", var_data(&monthly_minimum_charges[0], 12));
				assign("year1_monthly_dc_fixed_with_system", var_data(&monthly_dc_fixed[0], 12));
				assign("year1_monthly_dc_tou_with_system", var_data(&monthly_dc_tou[0], 12));
				assign("year1_monthly_ec_charge_with_system", var_data(&monthly_ec_charges[0], 12));
				assign("year1_monthly_ec_charge_gross_with_system", var_data(&monthly_ec_charges_gross[0], 12));
				assign("year1_excess_dollars_applied", var_data(&monthly_excess_dollars_applied[0], 12));
				assign("year1_excess_dollars_earned", var_data(&monthly_excess_dollars_earned[0], 12));
				assign("year1_excess_kwhs_applied", var_data(&monthly_excess_kwhs_applied[0], 12));
				assign("year1_excess_kwhs_earned", var_data(&monthly_excess_kwhs_earned[0], 12));

			}
			
			// determine net-revenue benefit due to solar for year 'i'
			
			annual_net_revenue[i+1] = 0.0;
			annual_electric_load[i + 1] = 0.0;
			thermal_net[i + 1] = 0.0;
			annual_revenue_w_sys[i + 1] = 0.0;
			annual_revenue_wo_sys[i + 1] = 0.0;

			for (j = 0; j<m_num_rec_yearly; j++)
			{
				thermal_net[i + 1] +=  e_sys_cy[j];
				annual_net_revenue[i + 1] += revenue_w_sys[j] - revenue_wo_sys[j];
				annual_electric_load[i + 1] += -e_load_cy[j];
				annual_revenue_w_sys[i + 1] += revenue_w_sys[j];
				annual_revenue_wo_sys[i + 1] += revenue_wo_sys[j];
			}

			//Outputs from Paul, Nate and Sean 9/9/13
			annual_elec_cost_w_sys[i + 1] = -annual_revenue_w_sys[i+1];
			annual_elec_cost_wo_sys[i + 1] = -annual_revenue_wo_sys[i+1];


		}

		assign("elec_cost_with_system_year1", annual_elec_cost_w_sys[1]);
		assign("elec_cost_without_system_year1", annual_elec_cost_wo_sys[1]);
		assign("savings_year1", annual_elec_cost_wo_sys[1] - annual_elec_cost_w_sys[1]);
	}

	void monthly_outputs(ssc_number_t *e_load, ssc_number_t *e_sys, ssc_number_t *e_grid, ssc_number_t *salespurchases, ssc_number_t monthly_load[12], ssc_number_t monthly_generation[12], ssc_number_t monthly_elec_to_grid[12], ssc_number_t monthly_elec_needed_from_grid[12], ssc_number_t monthly_salespurchases[12])
	{
		// calculate the monthly net energy and monthly hours
		int m,d,h,s;
		ssc_number_t energy_use[12]; // 12 months
		int c=0;

		size_t steps_per_hour = m_num_rec_yearly / 8760;
		for (m=0;m<12;m++)
		{
			energy_use[m] = 0;
			monthly_load[m] = 0;
			monthly_generation[m] = 0;
			monthly_elec_to_grid[m] = 0;
			monthly_salespurchases[m] = 0;
			for (d=0;d<util::nday[m];d++)
			{
				for(h=0;h<24;h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						// net energy use per month
						energy_use[m] += e_grid[c];
						// Sean's sign convention
						monthly_load[m] -= e_load[c];
						monthly_generation[m] += e_sys[c]; // does not include first year sys_scale
						monthly_elec_to_grid[m] += e_grid[c];
						// 9/10/13 update from Paul
						monthly_salespurchases[m] += salespurchases[c];
						c++;
					}
				}
			}
		}
		//
		
		for (m=0;m<12;m++)
		{
			if (monthly_elec_to_grid[m] > 0)
				monthly_elec_needed_from_grid[m] = monthly_elec_to_grid[m];
			else
				monthly_elec_needed_from_grid[m]=0;
		}
	}


	void tr_calc_timestep(ssc_number_t *e_in, ssc_number_t *p_in,
		ssc_number_t *revenue, ssc_number_t *payment, ssc_number_t *income,
		ssc_number_t *thermal_charge,
		ssc_number_t rate_esc, bool include_fixed = true, bool include_min = true, bool gen_only = false)
		throw(general_error)
	{
		int i;
		for (i = 0; i<(int)m_num_rec_yearly; i++)
			revenue[i] = payment[i] = income[i] = thermal_charge[i] = 0.0;

		 

		size_t steps_per_hour = m_num_rec_yearly / 8760;


		// calculate the monthly net energy and monthly hours
		int m, d, h, s, period, tier;
		size_t c = 0;
		for (m = 0; m < (int)m_month.size(); m++)
		{
			m_month[m].thermal_net = 0;
			m_month[m].hours_per_month = 0;
			m_month[m].thermal_peak = 0;
			m_month[m].thermal_peak_hour = 0;
			for (d = 0; d < util::nday[m]; d++)
			{
				for (h = 0; h < 24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
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
			for (d = 0; d<util::nday[m]; d++)
			{
				//daily_net_energy = 0;
				for (h = 0; h<24; h++)
				{
					for (s = 0; s < (int)steps_per_hour && c < (int)m_num_rec_yearly; s++)
					{
						{

							if (e_in[c] >= 0.0)
							{ // calculate income or credit

								// cumulative energy used to determine tier for credit of entire surplus amount
								ssc_number_t credit_amt = 0;
								ssc_number_t thermal_surplus = e_in[c];
								ssc_number_t sr = 0; // TODO: pull timestep rate setup above
								credit_amt = thermal_surplus * sr * rate_esc;
								// accumulate monthly charge and therms
							}
							else
							{ // calculate payment or charge
								ssc_number_t charge_amt = 0;
								ssc_number_t thermal_deficit = -e_in[c];
								ssc_number_t br = 0; // TODO: pull timestep rate setup above
								charge_amt = thermal_deficit * br * rate_esc;
								// accumulate monthly charge and therms

							}
						}
						// end of energy charge


						c++;
					} // steps per hour loop
				}  // h loop
			} // d loop

			// Calculate monthly bill 
		} // end of month m (m loop)


	}



};

DEFINE_MODULE_ENTRY( thermalrate, "Thermal flat rate structure net revenue calculator", 1 );


