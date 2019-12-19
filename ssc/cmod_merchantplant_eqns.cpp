#include <exception>
#include <algorithm>
#include <math.h>

#include "vartab.h"
#include "../shared/lib_util.h"
#include "../shared/lib_time.h"

#include "cmod_merchantplant_eqns.h"
//#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void mp_ancillary_services(ssc_data_t data)
{
	std::string error = "";
	bool ancillary_services_success = false;
	bool calculate_revenue = false;
	auto vt = static_cast<var_table*>(data);
	try {
		if (!vt) {
			throw std::runtime_error("ssc_data_t data invalid");
		}
	}
	catch (std::exception& e)
	{
		error = std::string(e.what());
	}
	try {
		bool gen_is_assigned = false;

		int mp_enable_energy_market_revenue, mp_enable_ancserv1, mp_enable_ancserv2, mp_enable_ancserv3, mp_enable_ancserv4;
		int mp_calculate_revenue;
		ssc_number_t analysis_period, system_capacity;
		util::matrix_t<ssc_number_t> mp_energy_market_revenue, mp_ancserv1_revenue, mp_ancserv2_revenue, mp_ancserv3_revenue, mp_ancserv4_revenue, system_gen;
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
        vt_get_number(vt, "analysis_period", &analysis_period);
        vt_get_int(vt, "mp_enable_energy_market_revenue", &mp_enable_energy_market_revenue);
        vt_get_int(vt, "mp_enable_ancserv1", &mp_enable_ancserv1);
        vt_get_int(vt, "mp_enable_ancserv2", &mp_enable_ancserv2);
        vt_get_int(vt, "mp_enable_ancserv3", &mp_enable_ancserv3);
        vt_get_int(vt, "mp_enable_ancserv4", &mp_enable_ancserv4);
        vt_get_matrix(vt, "mp_energy_market_revenue", mp_energy_market_revenue);
        vt_get_matrix(vt, "mp_ancserv1_revenue", mp_ancserv1_revenue);
        vt_get_matrix(vt, "mp_ancserv2_revenue", mp_ancserv2_revenue);
        vt_get_matrix(vt, "mp_ancserv3_revenue", mp_ancserv3_revenue);
        vt_get_matrix(vt, "mp_ancserv4_revenue", mp_ancserv4_revenue);
        gen_is_assigned = (vt->lookup("gen") != NULL);
		if (gen_is_assigned)
		{
			system_capacity = 0.0;
            vt_get_matrix(vt, "gen", system_gen);
		}
		else
		{
            vt_get_number(vt, "system_capacity", &system_capacity);
		}
		calculate_revenue = (vt->lookup("mp_calculate_revenue") != NULL);
		if (calculate_revenue)
		{
            vt_get_int(vt, "mp_calculate_revenue", &mp_calculate_revenue);
            calculate_revenue = (bool)mp_calculate_revenue;
		}

		// kW to MW for comparison
		system_capacity /= 1000.0;
		// sum up all power values from all revenue inputs and find smallest timestep and compare sum to system capacity
		bool en_mp_energy_market = (mp_enable_energy_market_revenue > 0.5);
		bool en_mp_ancserv1 = (mp_enable_ancserv1 > 0.5);
		bool en_mp_ancserv2 = (mp_enable_ancserv2 > 0.5);
		bool en_mp_ancserv3 = (mp_enable_ancserv3 > 0.5);
		bool en_mp_ancserv4 = (mp_enable_ancserv4 > 0.5);
		// if none enabled then check passes
		ancillary_services_success = (!mp_enable_energy_market_revenue && !mp_enable_ancserv1 && !mp_enable_ancserv2 && !mp_enable_ancserv3 && !mp_enable_ancserv4);

		size_t nsteps = 0, nsteps_per_year = 8760;
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

		if (nsteps < (8760 * (size_t)analysis_period)) nsteps = 8760 * (size_t)analysis_period; // extrapolated timeseries has minimum of hourly values for use in all forecasting 

		std::vector<ssc_number_t> energy_market_revenue(nsteps, 0.0);
		std::vector<ssc_number_t> ancillary_services1_revenue(nsteps, 0.0);
		std::vector<ssc_number_t> ancillary_services2_revenue(nsteps, 0.0);
		std::vector<ssc_number_t> ancillary_services3_revenue(nsteps, 0.0);
		std::vector<ssc_number_t> ancillary_services4_revenue(nsteps, 0.0);

		if (!ancillary_services_success)
		{
			// cleared capacity and price columns
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
					if (nsteps < (8760 * (size_t)analysis_period)) nsteps = 8760 * (size_t)analysis_period; // extrapolated timeseries has minimum of hourly values for use in all forecasting 
					std::vector<ssc_number_t> cleared_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> system_generation(nsteps, 0.0);
					std::vector<ssc_number_t> energy_market_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> ancillary_services1_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> ancillary_services2_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> ancillary_services3_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> ancillary_services4_capacity(nsteps, 0.0);
					std::vector<ssc_number_t> current_year_capacity;
					std::vector<ssc_number_t> extrapolated_current_year_capacity;
					std::vector<ssc_number_t> current_year_revenue;
					std::vector<ssc_number_t> extrapolated_current_year_revenue;
					nsteps_per_year = nsteps / (size_t)analysis_period;
					if (nsteps_per_year < 8760) nsteps_per_year = 8760; // for use of extrapolated_timeseries

					size_t steps_per_hour = nsteps_per_year / 8760;
					size_t current_num_per_year;
					for (size_t iyear = 0; iyear < (size_t)analysis_period; iyear++)
					{
						if (gen_is_assigned)
						{
							current_year_capacity.clear();
							current_num_per_year = system_gen.ncols() / (size_t)analysis_period;
							current_year_capacity.reserve(current_num_per_year);
							for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < system_gen.ncols()); ic++)
								current_year_capacity.push_back(system_gen(0, ic + iyear * current_num_per_year)/1000.0); // kW to MW
							extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
							for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
								system_generation[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
						}
						else
						{
							for (size_t ic = 0; (ic < nsteps_per_year); ic++)
								system_generation[ic + iyear * nsteps_per_year] = system_capacity;
						}
						if (en_mp_energy_market)
						{
							current_year_capacity.clear();
							current_num_per_year = mp_energy_market_revenue.nrows() / (size_t)analysis_period;
							current_year_capacity.reserve(current_num_per_year);
							for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_energy_market_revenue.nrows()); ic++)
								current_year_capacity.push_back(mp_energy_market_revenue(ic + iyear * current_num_per_year, 0));
							extrapolated_current_year_capacity = extrapolate_timeseries(current_year_capacity, steps_per_hour);
							for (size_t ic = 0; (ic < extrapolated_current_year_capacity.size()) && ((ic + iyear * current_num_per_year) < cleared_capacity.size()); ic++)
							{
								energy_market_capacity[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
								cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
							}

							if (calculate_revenue)
							{
								current_year_revenue.clear();
								current_year_revenue.reserve(current_num_per_year);
								for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_energy_market_revenue.nrows()); ic++)
									current_year_revenue.push_back(mp_energy_market_revenue(ic + iyear * current_num_per_year, 1));
								extrapolated_current_year_revenue = extrapolate_timeseries(current_year_revenue, steps_per_hour);
								for (size_t ic = 0; (ic < extrapolated_current_year_revenue.size()) && ((ic + iyear * current_num_per_year) < energy_market_revenue.size()); ic++)
									energy_market_revenue[ic + iyear * nsteps_per_year] = extrapolated_current_year_revenue[ic]; // $/MWh
							}

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
							{
								ancillary_services1_capacity[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
								cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
							}

							if (calculate_revenue)
							{
								current_year_revenue.clear();
								current_year_revenue.reserve(current_num_per_year);
								for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv1_revenue.nrows()); ic++)
									current_year_revenue.push_back(mp_ancserv1_revenue(ic + iyear * current_num_per_year, 1));
								extrapolated_current_year_revenue = extrapolate_timeseries(current_year_revenue, steps_per_hour);
								for (size_t ic = 0; (ic < extrapolated_current_year_revenue.size()) && ((ic + iyear * current_num_per_year) < ancillary_services1_revenue.size()); ic++)
									ancillary_services1_revenue[ic + iyear * nsteps_per_year] = extrapolated_current_year_revenue[ic]; // $/MWh
							}

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
							{
								ancillary_services2_capacity[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
								cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
							}

							if (calculate_revenue)
							{
								current_year_revenue.clear();
								current_year_revenue.reserve(current_num_per_year);
								for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv2_revenue.nrows()); ic++)
									current_year_revenue.push_back(mp_ancserv2_revenue(ic + iyear * current_num_per_year, 1));
								extrapolated_current_year_revenue = extrapolate_timeseries(current_year_revenue, steps_per_hour);
								for (size_t ic = 0; (ic < extrapolated_current_year_revenue.size()) && ((ic + iyear * current_num_per_year) < ancillary_services2_revenue.size()); ic++)
									ancillary_services2_revenue[ic + iyear * nsteps_per_year] = extrapolated_current_year_revenue[ic]; // $/MWh
							}

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
							{
								ancillary_services3_capacity[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
								cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
							}

							if (calculate_revenue)
							{
								current_year_revenue.clear();
								current_year_revenue.reserve(current_num_per_year);
								for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv3_revenue.nrows()); ic++)
									current_year_revenue.push_back(mp_ancserv3_revenue(ic + iyear * current_num_per_year, 1));
								extrapolated_current_year_revenue = extrapolate_timeseries(current_year_revenue, steps_per_hour);
								for (size_t ic = 0; (ic < extrapolated_current_year_revenue.size()) && ((ic + iyear * current_num_per_year) < ancillary_services3_revenue.size()); ic++)
									ancillary_services3_revenue[ic + iyear * nsteps_per_year] = extrapolated_current_year_revenue[ic]; // $/MWh
							}

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
							{
								ancillary_services4_capacity[ic + iyear * nsteps_per_year] = extrapolated_current_year_capacity[ic];
								cleared_capacity[ic + iyear * nsteps_per_year] += extrapolated_current_year_capacity[ic];
							}

							if (calculate_revenue)
							{
								current_year_revenue.clear();
								current_year_revenue.reserve(current_num_per_year);
								for (size_t ic = 0; (ic < current_num_per_year) && ((ic + iyear * current_num_per_year) < mp_ancserv4_revenue.nrows()); ic++)
									current_year_revenue.push_back(mp_ancserv4_revenue(ic + iyear * current_num_per_year, 1));
								extrapolated_current_year_revenue = extrapolate_timeseries(current_year_revenue, steps_per_hour);
								for (size_t ic = 0; (ic < extrapolated_current_year_revenue.size()) && ((ic + iyear * current_num_per_year) < ancillary_services4_revenue.size()); ic++)
									ancillary_services4_revenue[ic + iyear * nsteps_per_year] = extrapolated_current_year_revenue[ic]; // $/MWh
							}

						}
					}
					// check each timestep against system capacity
					if (cleared_capacity.size() != system_generation.size())
						error = util::format("cleared capacity size %d and capacity check size %d do not match", int(cleared_capacity.size()), int(system_generation.size()));
					else
					{
						for (size_t i = 0; (i < cleared_capacity.size()) && (i < system_generation.size()); i++)
						{
							if ((cleared_capacity[i] > 0) && (cleared_capacity[i] > system_generation[i]))
							{
								error = util::format("sum of cleared capacity %g exceeds system capacity %g at timestep %d", cleared_capacity[i], system_generation[i], int(i));
								break;
							}
						}
					}

					if (calculate_revenue)
					{ // apply in order and check for power left and apply in next market as necessary system_generation - market cap for current ancillary service
						if (en_mp_energy_market)
						{
							if (system_generation.size() != energy_market_revenue.size())
								error = util::format("system generation size %d and energy market revenue size %d do not match", int(system_generation.size()), int(energy_market_revenue.size()));
							else
							{
								for (size_t i = 0; (i < system_generation.size()) && (i < energy_market_capacity.size()) && (i < energy_market_revenue.size()); i++)
								{
									if (system_generation[i] > energy_market_capacity[i])
									{
										energy_market_revenue[i] *= energy_market_capacity[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
										system_generation[i] -= energy_market_capacity[i];
									}
									else
									{
										energy_market_revenue[i] *= system_generation[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
									}
								}
							}
						}
						if (en_mp_ancserv1)
						{
							if (system_generation.size() != ancillary_services1_revenue.size())
								error = util::format("system generation size %d and ancillary services1 revenue revenue size %d do not match", int(system_generation.size()), int(ancillary_services1_revenue.size()));
							else
							{
								for (size_t i = 0; (i < system_generation.size()) && (i < ancillary_services1_capacity.size()) && (i < ancillary_services1_revenue.size()); i++)
								{
									if (system_generation[i] > ancillary_services1_capacity[i])
									{
										ancillary_services1_revenue[i] *= ancillary_services1_capacity[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
										system_generation[i] -= ancillary_services1_capacity[i];
									}
									else
									{
										ancillary_services1_revenue[i] *= system_generation[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
									}
								}
							}
						}
						if (en_mp_ancserv2)
						{
							if (system_generation.size() != ancillary_services2_revenue.size())
								error = util::format("system generation size %d and ancillary services2 revenue revenue size %d do not match", int(system_generation.size()), int(ancillary_services2_revenue.size()));
							else
							{
								for (size_t i = 0; (i < system_generation.size()) && (i < ancillary_services2_capacity.size()) && (i < ancillary_services2_revenue.size()); i++)
								{
									if (system_generation[i] > ancillary_services2_capacity[i])
									{
										ancillary_services2_revenue[i] *= ancillary_services2_capacity[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
										system_generation[i] -= ancillary_services2_capacity[i];
									}
									else
									{
										ancillary_services2_revenue[i] *= system_generation[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
									}
								}
							}
						}
						if (en_mp_ancserv3)
						{
							if (system_generation.size() != ancillary_services3_revenue.size())
								error = util::format("system generation size %d and ancillary services3 revenue revenue size %d do not match", int(system_generation.size()), int(ancillary_services3_revenue.size()));
							else
							{
								for (size_t i = 0; (i < system_generation.size()) && (i < ancillary_services3_capacity.size()) && (i < ancillary_services3_revenue.size()); i++)
								{
									if (system_generation[i] > ancillary_services3_capacity[i])
									{
										ancillary_services3_revenue[i] *= ancillary_services3_capacity[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
										system_generation[i] -= ancillary_services3_capacity[i];
									}
									else
									{
										ancillary_services3_revenue[i] *= system_generation[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
									}
								}
							}
						}
						if (en_mp_ancserv4)
						{
							if (system_generation.size() != ancillary_services4_revenue.size())
								error = util::format("system generation size %d and ancillary services4 revenue revenue size %d do not match", int(system_generation.size()), int(ancillary_services4_revenue.size()));
							else
							{
								for (size_t i = 0; (i < system_generation.size()) && (i < ancillary_services4_capacity.size()) && (i < ancillary_services4_revenue.size()); i++)
								{
									if (system_generation[i] > ancillary_services4_capacity[i])
									{
										ancillary_services4_revenue[i] *= ancillary_services4_capacity[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
										system_generation[i] -= ancillary_services4_capacity[i];
									}
									else
									{
										ancillary_services4_revenue[i] *= system_generation[i] / steps_per_hour; // [MW] * [$/MWh] / fraction per hour [1/h]
									}
								}
							}
						}
					}

				}
				else
					error = util::format("Invalid number of timesteps requested %d", int(analysis_period));
			}
			else
				error = util::format("Invalid analysis period %d", int(analysis_period));
		}
		// expected outputs regardless of which markets enabled

		var_data mp_energy_market_generated_revenue = var_data(energy_market_revenue.data(), energy_market_revenue.size());
		vt->assign("mp_energy_market_generated_revenue", mp_energy_market_generated_revenue);
		var_data mp_ancillary_services1_generated_revenue = var_data(ancillary_services1_revenue.data(), ancillary_services1_revenue.size());
		vt->assign("mp_ancillary_services1_generated_revenue", mp_ancillary_services1_generated_revenue);
		var_data mp_ancillary_services2_generated_revenue = var_data(ancillary_services2_revenue.data(), ancillary_services2_revenue.size());
		vt->assign("mp_ancillary_services2_generated_revenue", mp_ancillary_services2_generated_revenue);
		var_data mp_ancillary_services3_generated_revenue = var_data(ancillary_services3_revenue.data(), ancillary_services3_revenue.size());
		vt->assign("mp_ancillary_services3_generated_revenue", mp_ancillary_services3_generated_revenue);
		var_data mp_ancillary_services4_generated_revenue = var_data(ancillary_services4_revenue.data(), ancillary_services4_revenue.size());
		vt->assign("mp_ancillary_services4_generated_revenue", mp_ancillary_services4_generated_revenue);


	}
	catch (std::exception& e)
	{
		error = std::string(e.what());
	}
	ancillary_services_success = (error == "");
	vt->assign("mp_ancillary_services", var_data(ancillary_services_success));
	vt->assign("mp_ancillary_services_error", var_data(error));

}



