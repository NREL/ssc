#include <algorithm>
#include <math.h>

#include "vartab.h"
#include "../shared/lib_util.h"
#include "../shared/lib_time.h"

#include "cmod_merchantplant_eqns.h"

void mp_capacity_check(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }
	ssc_number_t analysis_period, system_capacity, mp_enable_energy_market_revenue, mp_enable_ancserv1, mp_enable_ancserv2, mp_enable_ancserv3, mp_enable_ancserv4;
	util::matrix_t<ssc_number_t> mp_energy_market_revenue, mp_ancserv1_revenue, mp_ancserv2_revenue, mp_ancserv3_revenue, mp_ancserv4_revenue;
	/*
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_energy_market_revenue",		      "Enable energy market revenue",   "0/1",   "",    "",  "*",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT, SSC_MATRIX, "mp_energy_market_revenue", "Energy market revenue input", "", "","*", "", ""},
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv1",		      "Enable ancillary services 1 revenue",   "0/1",   "",    "",  "*",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT, SSC_MATRIX, "mp_ancserv1_revenue", "Ancillary services 1 revenue input", "", "","*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv2",		      "Enable ancillary services 2 revenue",   "0/1",   "",    "",  "*",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT, SSC_MATRIX, "mp_ancserv2_revenue", "Ancillary services 2 revenue input", "", "","*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv3",		      "Enable ancillary services 3 revenue",   "0/1",   "",    "",  "*",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT, SSC_MATRIX, "mp_ancserv3_revenue", "Ancillary services 3 revenue input", "", "","*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,     "mp_enable_ancserv4",		      "Enable ancillary services 4 revenue",   "0/1",   "",    "",  "*",	"INTEGER,MIN=0,MAX=1",      "" },
	{ SSC_INPUT, SSC_MATRIX, "mp_ancserv4_revenue", "Ancillary services 4 revenue input", "", "","*", "", "" },
	*/
	VT_GET_INPUT(vt, "analysis_period", analysis_period)
		VT_GET_INPUT(vt, "system_capacity", system_capacity)
		VT_GET_INPUT(vt, "mp_enable_energy_market_revenue", mp_enable_energy_market_revenue)
		VT_GET_INPUT(vt, "mp_enable_ancserv1", mp_enable_ancserv1)
		VT_GET_INPUT(vt, "mp_enable_ancserv2", mp_enable_ancserv2)
		VT_GET_INPUT(vt, "mp_enable_ancserv3", mp_enable_ancserv3)
		VT_GET_INPUT(vt, "mp_enable_ancserv4", mp_enable_ancserv4)
		VT_GET_INPUT(vt, "mp_energy_market_revenue", mp_energy_market_revenue)
		VT_GET_INPUT(vt, "mp_ancserv1_revenue", mp_ancserv1_revenue)
		VT_GET_INPUT(vt, "mp_ancserv2_revenue", mp_ancserv2_revenue)
		VT_GET_INPUT(vt, "mp_ancserv3_revenue", mp_ancserv3_revenue)
		VT_GET_INPUT(vt, "mp_ancserv4_revenue", mp_ancserv4_revenue)

		// kW to MW for comparison
		system_capacity /= 1000.0;
	// sum up all power values from all revenue inputs and find smallest timestep and compare sum to system capacity
	bool mp_capacity_check = false;
	bool en_mp_energy_market = (mp_enable_energy_market_revenue > 0.5);
	bool en_mp_ancserv1 = (mp_enable_ancserv1 > 0.5);
	bool en_mp_ancserv2 = (mp_enable_ancserv2 > 0.5);
	bool en_mp_ancserv3 = (mp_enable_ancserv3 > 0.5);
	bool en_mp_ancserv4 = (mp_enable_ancserv4 > 0.5);
	// if none enabled then check passes
	mp_capacity_check = (!mp_enable_energy_market_revenue && !mp_enable_ancserv1 && !mp_enable_ancserv2 && !mp_enable_ancserv3 && !mp_enable_ancserv4);
	std::string error = "";

	if (!mp_capacity_check)
	{
		// cleared capacity and price columns
		size_t nsteps = 0, nsteps_per_year = 8760;
		if (analysis_period > 0)
		{
			if (en_mp_energy_market)
				nsteps = std::max(nsteps, mp_energy_market_revenue.nrows());
			if (en_mp_ancserv1)
				nsteps = std::max(nsteps, mp_ancserv1_revenue.nrows());
			if (en_mp_ancserv2)
				nsteps = std::max(nsteps, mp_ancserv2_revenue.nrows());
			if (en_mp_ancserv3)
				nsteps = std::max(nsteps, mp_ancserv3_revenue.nrows());
			if (en_mp_ancserv4)
				nsteps = std::max(nsteps, mp_ancserv4_revenue.nrows());

			if (nsteps > 0)
			{
				if (nsteps < (8760 * analysis_period)) nsteps = 8760 * analysis_period; // extrapolated timeseries has minimum of hourly values for use in all forecasting 
				std::vector<ssc_number_t> cleared_capacity(nsteps, 0.0);
				std::vector<ssc_number_t> current_year_capacity;
				std::vector<ssc_number_t> extrapolated_current_year_capacity;
				nsteps_per_year = nsteps / (size_t)analysis_period;
				if (nsteps_per_year < 8760) nsteps_per_year = 8760; // for use of extrapolated_timeseries

				size_t steps_per_hour = nsteps_per_year / 8760;
				size_t current_num_per_year;
				for (size_t iyear = 0; iyear < (size_t)analysis_period; iyear++)
				{
					if (en_mp_energy_market)
					{
						current_year_capacity.clear();
						current_num_per_year = mp_energy_market_revenue.nrows() / (size_t)analysis_period;
						current_year_capacity.reserve(current_num_per_year);
						for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_energy_market_revenue.nrows()); ic++)
							current_year_capacity.push_back(mp_energy_market_revenue(ic + iyear * current_num_per_year, 0));
						extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
						for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
					}
					if (en_mp_ancserv1)
					{
						current_year_capacity.clear();
						current_num_per_year = mp_ancserv1_revenue.nrows() / (size_t)analysis_period;
						current_year_capacity.reserve(current_num_per_year);
						for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv1_revenue.nrows()); ic++)
							current_year_capacity.push_back(mp_ancserv1_revenue(ic + iyear * current_num_per_year, 0));
						extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
						for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
					}
					if (en_mp_ancserv2)
					{
						current_year_capacity.clear();
						current_num_per_year = mp_ancserv2_revenue.nrows() / (size_t)analysis_period;
						current_year_capacity.reserve(current_num_per_year);
						for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv2_revenue.nrows()); ic++)
							current_year_capacity.push_back(mp_ancserv2_revenue(ic + iyear * current_num_per_year, 0));
						extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
						for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
					}
					if (en_mp_ancserv3)
					{
						current_year_capacity.clear();
						current_num_per_year = mp_ancserv3_revenue.nrows() / (size_t)analysis_period;
						current_year_capacity.reserve(current_num_per_year);
						for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv3_revenue.nrows()); ic++)
							current_year_capacity.push_back(mp_ancserv3_revenue(ic + iyear * current_num_per_year, 0));
						extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
						for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
					}
					if (en_mp_ancserv4)
					{
						current_year_capacity.clear();
						current_num_per_year = mp_ancserv4_revenue.nrows() / (size_t)analysis_period;
						current_year_capacity.reserve(current_num_per_year);
						for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv4_revenue.nrows()); ic++)
							current_year_capacity.push_back(mp_ancserv4_revenue(ic + iyear * current_num_per_year, 0));
						extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
						for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
					}
				}
				// check each timestep against system capacity
				for (size_t i = 0; i < cleared_capacity.size(); i++)
				{
					if (cleared_capacity[i] > system_capacity)
					{
						error = util::format("Sum of cleared capacity %g exceeds system capacity %g at timestep %d", cleared_capacity[i], system_capacity, int(i));
						break;
					}
				}
				mp_capacity_check = (error == "");
			}
			else
				error = util::format("Invalid number of timesteps requested %d", int(analysis_period));
		}
		else
			error = util::format("Invalid analysis period %d", int(analysis_period));
	}
	vt->assign("mp_capacity_check", var_data(mp_capacity_check));
	vt->assign("mp_capacity_check_error", var_data(error));
}



