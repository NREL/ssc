#include "lib_utility_rate_equations.h"

void rate_data::init_energy_rates(bool gen_only) {
	// calculate the monthly net energy per tier and period based on units
	int c = 0;
	for (int m = 0; m < (int)m_month.size(); m++)
	{
		// check for kWh/kW
		int start_tier = 0;
		int end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;
		int num_periods = (int)m_month[m].ec_tou_ub.nrows();
		int num_tiers = end_tier - start_tier + 1;

		if (!gen_only) // added for two meter no load scenarios to use load tier sizing
		{
			//start_tier = 0;
			end_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
			//int num_periods = (int)m_month[m].ec_tou_ub_init.nrows();
			num_tiers = end_tier - start_tier + 1;


			// kWh/kW (kWh/kW daily handled in Setup)
			// 1. find kWh/kW tier
			// 2. set min tier and max tier based on next item in ec_tou matrix
			// 3. resize use and chart based on number of tiers in kWh/kW section
			// 4. assumption is that all periods in same month have same tier breakdown
			// 5. assumption is that tier numbering is correct for the kWh/kW breakdown
			// That is, first tier must be kWh/kW
			if ((m_month[m].ec_tou_units.ncols() > 0 && m_month[m].ec_tou_units.nrows() > 0)
				&& ((m_month[m].ec_tou_units.at(0, 0) == 1) || (m_month[m].ec_tou_units.at(0, 0) == 3)))
			{
				// monthly total energy / monthly peak to determine which kWh/kW tier
				double mon_kWhperkW = -m_month[m].energy_net; // load negative
				if (m_month[m].dc_flat_peak != 0)
					mon_kWhperkW /= m_month[m].dc_flat_peak;
				// find correct start and end tier based on kWhperkW band
				start_tier = 1;
				bool found = false;
				for (size_t i_tier = 0; i_tier < m_month[m].ec_tou_units.ncols(); i_tier++)
				{
					int units = (int)m_month[m].ec_tou_units.at(0, i_tier);
					if ((units == 1) || (units == 3))
					{
						if (found)
						{
							end_tier = (int)i_tier - 1;
							break;
						}
						else if (mon_kWhperkW < m_month[m].ec_tou_ub_init.at(0, i_tier))
						{
							start_tier = (int)i_tier + 1;
							found = true;
						}
					}
				}
				// last tier since no max specified in rate
				if (!found) start_tier = end_tier;
				if (start_tier >= (int)m_month[m].ec_tou_ub_init.ncols())
					start_tier = (int)m_month[m].ec_tou_ub_init.ncols() - 1;
				if (end_tier < start_tier)
					end_tier = start_tier;
				num_tiers = end_tier - start_tier + 1;
				// resize everytime to handle load and energy changes
				// resize sr, br and ub for use in energy charge calculations below
				util::matrix_t<float> br(num_periods, num_tiers);
				util::matrix_t<float> sr(num_periods, num_tiers);
				util::matrix_t<float> ub(num_periods, num_tiers);
				// assign appropriate values.
				for (int period = 0; period < num_periods; period++)
				{
					for (int tier = 0; tier < num_tiers; tier++)
					{
						br.at(period, tier) = m_month[m].ec_tou_br_init.at(period, start_tier + tier);
						sr.at(period, tier) = m_month[m].ec_tou_sr_init.at(period, start_tier + tier);
						ub.at(period, tier) = m_month[m].ec_tou_ub_init.at(period, start_tier + tier);
						// update for correct tier number column headings
						m_month[m].ec_periods_tiers[period][tier] = start_tier + m_ec_periods_tiers_init[period][tier];
					}
				}

				m_month[m].ec_tou_br = br;
				m_month[m].ec_tou_sr = sr;
				m_month[m].ec_tou_ub = ub;
			}
		}
		// reset now resized
		start_tier = 0;
		end_tier = (int)m_month[m].ec_tou_ub.ncols() - 1;

		m_month[m].ec_energy_surplus.resize_fill(num_periods, num_tiers, 0);
		m_month[m].ec_energy_use.resize_fill(num_periods, num_tiers, 0);
		m_month[m].ec_charge.resize_fill(num_periods, num_tiers, 0);

	}
}