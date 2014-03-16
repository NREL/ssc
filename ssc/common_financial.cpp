#include "common_financial.h"
#include "core.h"
#include <sstream>
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif




//var_info vtab_dispatch_periods[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor1", "Dispatch period 1 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor2", "Dispatch period 2 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor3", "Dispatch period 3 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor4", "Dispatch period 4 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor5", "Dispatch period 5 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor6", "Dispatch period 6 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor7", "Dispatch period 7 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor8", "Dispatch period 8 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor9", "Dispatch period 9 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
//	{ SSC_INPUT, SSC_MATRIX, "dispatch_sched_weekday", "Diurnal weekday dispatch periods", "1..9", "12 x 24 matrix", "Dispatch values", "*", "", "" },
//	{ SSC_INPUT, SSC_MATRIX, "dispatch_sched_weekend", "Diurnal weekend dispatch periods", "1..9", "12 x 24 matrix", "Dispatch values", "*", "", "" },

//	var_info_invalid };


dispatch_periods::dispatch_periods(compute_module *cm, std::vector<double>& degradation)
: m_cm(cm), m_degradation(degradation)
{
	setup();
	m_nyears = m_cm->as_integer("analysis_period");
	if (m_cm->as_integer("system_use_lifetime_output"))
		process_lifetime_dispatch_output(m_nyears);
	else
		process_dispatch_output(m_nyears);


}

util::matrix_t<double>& dispatch_periods::dispatch_output()
{
	return m_cf;
}


bool dispatch_periods::setup()
{


	// initialize cashflow matrix
	m_cf.resize_fill(CF_max_dispatch, m_nyears + 1, 0.0);

	size_t nrows, ncols;
	ssc_number_t *disp_weekday = m_cm->as_matrix("dispatch_sched_weekday", &nrows, &ncols);
	if (nrows != 12 || ncols != 24)
	{
		m_error = util::format("dispatch values weekday schedule must be 12x24, input is %dx%d", (int)nrows, (int)ncols);
		throw compute_module::exec_error("dispatch_values", m_error);
	}
	ssc_number_t *disp_weekend = m_cm->as_matrix("dispatch_sched_weekend", &nrows, &ncols);
	if (nrows != 12 || ncols != 24)
	{
		m_error = util::format("dispatch values weekend schedule must be 12x24, input is %dx%d", (int)nrows, (int)ncols);
		throw compute_module::exec_error("dispatch_values", m_error);
	}
	util::matrix_t<float> schedwkday(12, 24);
	schedwkday.assign(disp_weekday, nrows, ncols);
	util::matrix_t<float> schedwkend(12, 24);
	schedwkend.assign(disp_weekend, nrows, ncols);

	int tod[8760];

	if (!util::translate_schedule(tod, schedwkday, schedwkend, 1, 9))
	{
		m_error = "could not translate weekday and weekend schedules for dispatch values";
		throw compute_module::general_error(m_error);
	}

	m_periods.resize(8760, 1);
	for (int i = 0; i < 8760; i++)
		m_periods[i] = tod[i];
//	{
		//switch (tod[i])
		//{
		//case 1:
		//	m_factors[i] = m_cm->as_number("dispatch_factor1");
		//	break;
		//case 2:
		//	m_factors[i] = m_cm->as_number("dispatch_factor2");
		//	break;
		//case 3:
		//	m_factors[i] = m_cm->as_number("dispatch_factor3");
		//	break;
		//case 4:
		//	m_factors[i] = m_cm->as_number("dispatch_factor4");
		//	break;
		//case 5:
		//	m_factors[i] = m_cm->as_number("dispatch_factor5");
		//	break;
		//case 6:
		//	m_factors[i] = m_cm->as_number("dispatch_factor6");
		//	break;
		//case 7:
		//	m_factors[i] = m_cm->as_number("dispatch_factor7");
		//	break;
		//case 8:
		//	m_factors[i] = m_cm->as_number("dispatch_factor8");
		//	break;
		//case 9:
		//	m_factors[i] = m_cm->as_number("dispatch_factor9");
		//	break;
		//}
//	}


	return m_error.length() == 0;
}

int dispatch_periods::operator()(size_t time)
{
	if (time < m_periods.size()) return m_periods[time];
	else return 1;
}



bool dispatch_periods::compute_dispatch_output(int nyears)
{
	//Calculate energy dispatched in each dispatch period 
	//ssc_number_t *m_periods; // tou period 
	ssc_number_t *hourly_enet; // hourly energy output


	int h;
	size_t count;

	// hourly energy

	hourly_enet =m_cm->as_array("hourly_energy", &count);
	if (count != 8760)
	{
		std::stringstream outm;
		outm << "Bad hourly energy output length (" << count << "), should be 8760 value";
		m_cm->log(outm.str());
		return false;
	}


	m_cf.at(CF_TOD1Energy, 1) = 0;
	m_cf.at(CF_TOD2Energy, 1) = 0;
	m_cf.at(CF_TOD3Energy, 1) = 0;
	m_cf.at(CF_TOD4Energy, 1) = 0;
	m_cf.at(CF_TOD5Energy, 1) = 0;
	m_cf.at(CF_TOD6Energy, 1) = 0;
	m_cf.at(CF_TOD7Energy, 1) = 0;
	m_cf.at(CF_TOD8Energy, 1) = 0;
	m_cf.at(CF_TOD9Energy, 1) = 0;


	// hourly net energy include first year curtailment, availability and degradation
	// unapply first year availability and degradation so that dispatch can be properly calculated and 
	// so that availability and degradation is not applied multiple times
	// Better would be to calculate dispatch energy in cmod_annual output; however, dispatch only
	// applies to IPP and DHF markets.



	for (h = 0; h<8760; h++)
	{
		switch (m_periods[h])
		{
		case 1:
			m_cf.at(CF_TOD1Energy, 1) += hourly_enet[h];
			break;
		case 2:
			m_cf.at(CF_TOD2Energy, 1) += hourly_enet[h];
			break;
		case 3:
			m_cf.at(CF_TOD3Energy, 1) += hourly_enet[h];
			break;
		case 4:
			m_cf.at(CF_TOD4Energy, 1) += hourly_enet[h];
			break;
		case 5:
			m_cf.at(CF_TOD5Energy, 1) += hourly_enet[h];
			break;
		case 6:
			m_cf.at(CF_TOD6Energy, 1) += hourly_enet[h];
			break;
		case 7:
			m_cf.at(CF_TOD7Energy, 1) += hourly_enet[h];
			break;
		case 8:
			m_cf.at(CF_TOD8Energy, 1) += hourly_enet[h];
			break;
		case 9:
			m_cf.at(CF_TOD9Energy, 1) += hourly_enet[h];
			break;
		}
	}
	// remove degradation and availability from year 1 values but keep curtailment so that
	// availability and degradation yearly schedules from cmod_annualoutput can be properly applied.
	double year1_TOD1Energy = m_cf.at(CF_TOD1Energy, 1);
	double year1_TOD2Energy = m_cf.at(CF_TOD2Energy, 1);
	double year1_TOD3Energy = m_cf.at(CF_TOD3Energy, 1);
	double year1_TOD4Energy = m_cf.at(CF_TOD4Energy, 1);
	double year1_TOD5Energy = m_cf.at(CF_TOD5Energy, 1);
	double year1_TOD6Energy = m_cf.at(CF_TOD6Energy, 1);
	double year1_TOD7Energy = m_cf.at(CF_TOD7Energy, 1);
	double year1_TOD8Energy = m_cf.at(CF_TOD8Energy, 1);
	double year1_TOD9Energy = m_cf.at(CF_TOD9Energy, 1);


	for (int y = 2; y <= nyears; y++)
	{
		// compute energy dispatched
		m_cf.at(CF_TOD1Energy, y) = year1_TOD1Energy * m_degradation[y]; 
		m_cf.at(CF_TOD2Energy, y) = year1_TOD2Energy * m_degradation[y]; 
		m_cf.at(CF_TOD3Energy, y) = year1_TOD3Energy * m_degradation[y]; 
		m_cf.at(CF_TOD4Energy, y) = year1_TOD4Energy * m_degradation[y]; 
		m_cf.at(CF_TOD5Energy, y) = year1_TOD5Energy * m_degradation[y]; 
		m_cf.at(CF_TOD6Energy, y) = year1_TOD6Energy * m_degradation[y]; 
		m_cf.at(CF_TOD7Energy, y) = year1_TOD7Energy * m_degradation[y]; 
		m_cf.at(CF_TOD8Energy, y) = year1_TOD8Energy * m_degradation[y]; 
		m_cf.at(CF_TOD9Energy, y) = year1_TOD9Energy * m_degradation[y]; 
	}


	return true;
}

bool dispatch_periods::process_dispatch_output(int nyears)
{
	//Calculate energy dispatched in each dispatch period 
	ssc_number_t *hourly_enet; // hourly energy output

	size_t count;

	// hourly energy
	hourly_enet = m_cm->as_array("hourly_energy", &count);
	if (count != 8760)
	{
		std::stringstream outm;
		outm << "Bad hourly energy output length (" << count << "), should be 8760 value";
		m_cm->log(outm.str());
		return false;
	}



	m_cf.at(CF_TODJanEnergy, 1) = 0;
	m_cf.at(CF_TODFebEnergy, 1) = 0;
	m_cf.at(CF_TODMarEnergy, 1) = 0;
	m_cf.at(CF_TODAprEnergy, 1) = 0;
	m_cf.at(CF_TODMayEnergy, 1) = 0;
	m_cf.at(CF_TODJunEnergy, 1) = 0;
	m_cf.at(CF_TODJulEnergy, 1) = 0;
	m_cf.at(CF_TODAugEnergy, 1) = 0;
	m_cf.at(CF_TODSepEnergy, 1) = 0;
	m_cf.at(CF_TODOctEnergy, 1) = 0;
	m_cf.at(CF_TODNovEnergy, 1) = 0;
	m_cf.at(CF_TODDecEnergy, 1) = 0;

	m_cf.at(CF_TOD1JanEnergy, 1) = 0;
	m_cf.at(CF_TOD1FebEnergy, 1) = 0;
	m_cf.at(CF_TOD1MarEnergy, 1) = 0;
	m_cf.at(CF_TOD1AprEnergy, 1) = 0;
	m_cf.at(CF_TOD1MayEnergy, 1) = 0;
	m_cf.at(CF_TOD1JunEnergy, 1) = 0;
	m_cf.at(CF_TOD1JulEnergy, 1) = 0;
	m_cf.at(CF_TOD1AugEnergy, 1) = 0;
	m_cf.at(CF_TOD1SepEnergy, 1) = 0;
	m_cf.at(CF_TOD1OctEnergy, 1) = 0;
	m_cf.at(CF_TOD1NovEnergy, 1) = 0;
	m_cf.at(CF_TOD1DecEnergy, 1) = 0;

	m_cf.at(CF_TOD2JanEnergy, 1) = 0;
	m_cf.at(CF_TOD2FebEnergy, 1) = 0;
	m_cf.at(CF_TOD2MarEnergy, 1) = 0;
	m_cf.at(CF_TOD2AprEnergy, 1) = 0;
	m_cf.at(CF_TOD2MayEnergy, 1) = 0;
	m_cf.at(CF_TOD2JunEnergy, 1) = 0;
	m_cf.at(CF_TOD2JulEnergy, 1) = 0;
	m_cf.at(CF_TOD2AugEnergy, 1) = 0;
	m_cf.at(CF_TOD2SepEnergy, 1) = 0;
	m_cf.at(CF_TOD2OctEnergy, 1) = 0;
	m_cf.at(CF_TOD2NovEnergy, 1) = 0;
	m_cf.at(CF_TOD2DecEnergy, 1) = 0;

	m_cf.at(CF_TOD3JanEnergy, 1) = 0;
	m_cf.at(CF_TOD3FebEnergy, 1) = 0;
	m_cf.at(CF_TOD3MarEnergy, 1) = 0;
	m_cf.at(CF_TOD3AprEnergy, 1) = 0;
	m_cf.at(CF_TOD3MayEnergy, 1) = 0;
	m_cf.at(CF_TOD3JunEnergy, 1) = 0;
	m_cf.at(CF_TOD3JulEnergy, 1) = 0;
	m_cf.at(CF_TOD3AugEnergy, 1) = 0;
	m_cf.at(CF_TOD3SepEnergy, 1) = 0;
	m_cf.at(CF_TOD3OctEnergy, 1) = 0;
	m_cf.at(CF_TOD3NovEnergy, 1) = 0;
	m_cf.at(CF_TOD3DecEnergy, 1) = 0;

	m_cf.at(CF_TOD4JanEnergy, 1) = 0;
	m_cf.at(CF_TOD4FebEnergy, 1) = 0;
	m_cf.at(CF_TOD4MarEnergy, 1) = 0;
	m_cf.at(CF_TOD4AprEnergy, 1) = 0;
	m_cf.at(CF_TOD4MayEnergy, 1) = 0;
	m_cf.at(CF_TOD4JunEnergy, 1) = 0;
	m_cf.at(CF_TOD4JulEnergy, 1) = 0;
	m_cf.at(CF_TOD4AugEnergy, 1) = 0;
	m_cf.at(CF_TOD4SepEnergy, 1) = 0;
	m_cf.at(CF_TOD4OctEnergy, 1) = 0;
	m_cf.at(CF_TOD4NovEnergy, 1) = 0;
	m_cf.at(CF_TOD4DecEnergy, 1) = 0;

	m_cf.at(CF_TOD5JanEnergy, 1) = 0;
	m_cf.at(CF_TOD5FebEnergy, 1) = 0;
	m_cf.at(CF_TOD5MarEnergy, 1) = 0;
	m_cf.at(CF_TOD5AprEnergy, 1) = 0;
	m_cf.at(CF_TOD5MayEnergy, 1) = 0;
	m_cf.at(CF_TOD5JunEnergy, 1) = 0;
	m_cf.at(CF_TOD5JulEnergy, 1) = 0;
	m_cf.at(CF_TOD5AugEnergy, 1) = 0;
	m_cf.at(CF_TOD5SepEnergy, 1) = 0;
	m_cf.at(CF_TOD5OctEnergy, 1) = 0;
	m_cf.at(CF_TOD5NovEnergy, 1) = 0;
	m_cf.at(CF_TOD5DecEnergy, 1) = 0;

	m_cf.at(CF_TOD6JanEnergy, 1) = 0;
	m_cf.at(CF_TOD6FebEnergy, 1) = 0;
	m_cf.at(CF_TOD6MarEnergy, 1) = 0;
	m_cf.at(CF_TOD6AprEnergy, 1) = 0;
	m_cf.at(CF_TOD6MayEnergy, 1) = 0;
	m_cf.at(CF_TOD6JunEnergy, 1) = 0;
	m_cf.at(CF_TOD6JulEnergy, 1) = 0;
	m_cf.at(CF_TOD6AugEnergy, 1) = 0;
	m_cf.at(CF_TOD6SepEnergy, 1) = 0;
	m_cf.at(CF_TOD6OctEnergy, 1) = 0;
	m_cf.at(CF_TOD6NovEnergy, 1) = 0;
	m_cf.at(CF_TOD6DecEnergy, 1) = 0;

	m_cf.at(CF_TOD7JanEnergy, 1) = 0;
	m_cf.at(CF_TOD7FebEnergy, 1) = 0;
	m_cf.at(CF_TOD7MarEnergy, 1) = 0;
	m_cf.at(CF_TOD7AprEnergy, 1) = 0;
	m_cf.at(CF_TOD7MayEnergy, 1) = 0;
	m_cf.at(CF_TOD7JunEnergy, 1) = 0;
	m_cf.at(CF_TOD7JulEnergy, 1) = 0;
	m_cf.at(CF_TOD7AugEnergy, 1) = 0;
	m_cf.at(CF_TOD7SepEnergy, 1) = 0;
	m_cf.at(CF_TOD7OctEnergy, 1) = 0;
	m_cf.at(CF_TOD7NovEnergy, 1) = 0;
	m_cf.at(CF_TOD7DecEnergy, 1) = 0;

	m_cf.at(CF_TOD8JanEnergy, 1) = 0;
	m_cf.at(CF_TOD8FebEnergy, 1) = 0;
	m_cf.at(CF_TOD8MarEnergy, 1) = 0;
	m_cf.at(CF_TOD8AprEnergy, 1) = 0;
	m_cf.at(CF_TOD8MayEnergy, 1) = 0;
	m_cf.at(CF_TOD8JunEnergy, 1) = 0;
	m_cf.at(CF_TOD8JulEnergy, 1) = 0;
	m_cf.at(CF_TOD8AugEnergy, 1) = 0;
	m_cf.at(CF_TOD8SepEnergy, 1) = 0;
	m_cf.at(CF_TOD8OctEnergy, 1) = 0;
	m_cf.at(CF_TOD8NovEnergy, 1) = 0;
	m_cf.at(CF_TOD8DecEnergy, 1) = 0;

	m_cf.at(CF_TOD9JanEnergy, 1) = 0;
	m_cf.at(CF_TOD9FebEnergy, 1) = 0;
	m_cf.at(CF_TOD9MarEnergy, 1) = 0;
	m_cf.at(CF_TOD9AprEnergy, 1) = 0;
	m_cf.at(CF_TOD9MayEnergy, 1) = 0;
	m_cf.at(CF_TOD9JunEnergy, 1) = 0;
	m_cf.at(CF_TOD9JulEnergy, 1) = 0;
	m_cf.at(CF_TOD9AugEnergy, 1) = 0;
	m_cf.at(CF_TOD9SepEnergy, 1) = 0;
	m_cf.at(CF_TOD9OctEnergy, 1) = 0;
	m_cf.at(CF_TOD9NovEnergy, 1) = 0;
	m_cf.at(CF_TOD9DecEnergy, 1) = 0;

	int i = 0;
	for (int m = 0; m<12; m++)
	{
		for (int d = 0; d<util::nday[m]; d++)
		{
			for (int h = 0; h<24 && i<8760 && m * 24 + h<288; h++)
			{
				switch (m)
				{
				case 0:
					m_cf.at(CF_TODJanEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JanEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JanEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JanEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JanEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JanEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JanEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JanEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JanEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JanEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 1:
					m_cf.at(CF_TODFebEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1FebEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2FebEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3FebEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4FebEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5FebEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6FebEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7FebEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8FebEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9FebEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 2:
					m_cf.at(CF_TODMarEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1MarEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2MarEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3MarEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4MarEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5MarEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6MarEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7MarEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8MarEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9MarEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 3:
					m_cf.at(CF_TODAprEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1AprEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2AprEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3AprEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4AprEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5AprEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6AprEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7AprEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8AprEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9AprEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 4:
					m_cf.at(CF_TODMayEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1MayEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2MayEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3MayEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4MayEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5MayEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6MayEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7MayEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8MayEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9MayEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 5:
					m_cf.at(CF_TODJunEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JunEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JunEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JunEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JunEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JunEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JunEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JunEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JunEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JunEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 6:
					m_cf.at(CF_TODJulEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JulEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JulEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JulEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JulEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JulEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JulEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JulEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JulEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JulEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 7:
					m_cf.at(CF_TODAugEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1AugEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2AugEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3AugEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4AugEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5AugEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6AugEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7AugEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8AugEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9AugEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 8:
					m_cf.at(CF_TODSepEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1SepEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2SepEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3SepEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4SepEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5SepEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6SepEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7SepEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8SepEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9SepEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 9:
					m_cf.at(CF_TODOctEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1OctEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2OctEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3OctEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4OctEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5OctEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6OctEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7OctEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8OctEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9OctEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 10:
					m_cf.at(CF_TODNovEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1NovEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2NovEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3NovEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4NovEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5NovEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6NovEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7NovEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8NovEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9NovEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				case 11:
					m_cf.at(CF_TODDecEnergy, 1) += hourly_enet[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1DecEnergy, 1) += hourly_enet[i];
						break;
					case 2:
						m_cf.at(CF_TOD2DecEnergy, 1) += hourly_enet[i];
						break;
					case 3:
						m_cf.at(CF_TOD3DecEnergy, 1) += hourly_enet[i];
						break;
					case 4:
						m_cf.at(CF_TOD4DecEnergy, 1) += hourly_enet[i];
						break;
					case 5:
						m_cf.at(CF_TOD5DecEnergy, 1) += hourly_enet[i];
						break;
					case 6:
						m_cf.at(CF_TOD6DecEnergy, 1) += hourly_enet[i];
						break;
					case 7:
						m_cf.at(CF_TOD7DecEnergy, 1) += hourly_enet[i];
						break;
					case 8:
						m_cf.at(CF_TOD8DecEnergy, 1) += hourly_enet[i];
						break;
					case 9:
						m_cf.at(CF_TOD9DecEnergy, 1) += hourly_enet[i];
						break;
					}
					break;
				}
				i++;
			}
		}
	}


	double year1_TODJanEnergy = m_cf.at(CF_TODJanEnergy, 1);
	double year1_TODFebEnergy = m_cf.at(CF_TODFebEnergy, 1);
	double year1_TODMarEnergy = m_cf.at(CF_TODMarEnergy, 1);
	double year1_TODAprEnergy = m_cf.at(CF_TODAprEnergy, 1);
	double year1_TODMayEnergy = m_cf.at(CF_TODMayEnergy, 1);
	double year1_TODJunEnergy = m_cf.at(CF_TODJunEnergy, 1);
	double year1_TODJulEnergy = m_cf.at(CF_TODJulEnergy, 1);
	double year1_TODAugEnergy = m_cf.at(CF_TODAugEnergy, 1);
	double year1_TODSepEnergy = m_cf.at(CF_TODSepEnergy, 1);
	double year1_TODOctEnergy = m_cf.at(CF_TODOctEnergy, 1);
	double year1_TODNovEnergy = m_cf.at(CF_TODNovEnergy, 1);
	double year1_TODDecEnergy = m_cf.at(CF_TODDecEnergy, 1);

	double year1_TOD1JanEnergy = m_cf.at(CF_TOD1JanEnergy, 1);
	double year1_TOD1FebEnergy = m_cf.at(CF_TOD1FebEnergy, 1);
	double year1_TOD1MarEnergy = m_cf.at(CF_TOD1MarEnergy, 1);
	double year1_TOD1AprEnergy = m_cf.at(CF_TOD1AprEnergy, 1);
	double year1_TOD1MayEnergy = m_cf.at(CF_TOD1MayEnergy, 1);
	double year1_TOD1JunEnergy = m_cf.at(CF_TOD1JunEnergy, 1);
	double year1_TOD1JulEnergy = m_cf.at(CF_TOD1JulEnergy, 1);
	double year1_TOD1AugEnergy = m_cf.at(CF_TOD1AugEnergy, 1);
	double year1_TOD1SepEnergy = m_cf.at(CF_TOD1SepEnergy, 1);
	double year1_TOD1OctEnergy = m_cf.at(CF_TOD1OctEnergy, 1);
	double year1_TOD1NovEnergy = m_cf.at(CF_TOD1NovEnergy, 1);
	double year1_TOD1DecEnergy = m_cf.at(CF_TOD1DecEnergy, 1);

	double year1_TOD2JanEnergy = m_cf.at(CF_TOD2JanEnergy, 1);
	double year1_TOD2FebEnergy = m_cf.at(CF_TOD2FebEnergy, 1);
	double year1_TOD2MarEnergy = m_cf.at(CF_TOD2MarEnergy, 1);
	double year1_TOD2AprEnergy = m_cf.at(CF_TOD2AprEnergy, 1);
	double year1_TOD2MayEnergy = m_cf.at(CF_TOD2MayEnergy, 1);
	double year1_TOD2JunEnergy = m_cf.at(CF_TOD2JunEnergy, 1);
	double year1_TOD2JulEnergy = m_cf.at(CF_TOD2JulEnergy, 1);
	double year1_TOD2AugEnergy = m_cf.at(CF_TOD2AugEnergy, 1);
	double year1_TOD2SepEnergy = m_cf.at(CF_TOD2SepEnergy, 1);
	double year1_TOD2OctEnergy = m_cf.at(CF_TOD2OctEnergy, 1);
	double year1_TOD2NovEnergy = m_cf.at(CF_TOD2NovEnergy, 1);
	double year1_TOD2DecEnergy = m_cf.at(CF_TOD2DecEnergy, 1);

	double year1_TOD3JanEnergy = m_cf.at(CF_TOD3JanEnergy, 1);
	double year1_TOD3FebEnergy = m_cf.at(CF_TOD3FebEnergy, 1);
	double year1_TOD3MarEnergy = m_cf.at(CF_TOD3MarEnergy, 1);
	double year1_TOD3AprEnergy = m_cf.at(CF_TOD3AprEnergy, 1);
	double year1_TOD3MayEnergy = m_cf.at(CF_TOD3MayEnergy, 1);
	double year1_TOD3JunEnergy = m_cf.at(CF_TOD3JunEnergy, 1);
	double year1_TOD3JulEnergy = m_cf.at(CF_TOD3JulEnergy, 1);
	double year1_TOD3AugEnergy = m_cf.at(CF_TOD3AugEnergy, 1);
	double year1_TOD3SepEnergy = m_cf.at(CF_TOD3SepEnergy, 1);
	double year1_TOD3OctEnergy = m_cf.at(CF_TOD3OctEnergy, 1);
	double year1_TOD3NovEnergy = m_cf.at(CF_TOD3NovEnergy, 1);
	double year1_TOD3DecEnergy = m_cf.at(CF_TOD3DecEnergy, 1);

	double year1_TOD4JanEnergy = m_cf.at(CF_TOD4JanEnergy, 1);
	double year1_TOD4FebEnergy = m_cf.at(CF_TOD4FebEnergy, 1);
	double year1_TOD4MarEnergy = m_cf.at(CF_TOD4MarEnergy, 1);
	double year1_TOD4AprEnergy = m_cf.at(CF_TOD4AprEnergy, 1);
	double year1_TOD4MayEnergy = m_cf.at(CF_TOD4MayEnergy, 1);
	double year1_TOD4JunEnergy = m_cf.at(CF_TOD4JunEnergy, 1);
	double year1_TOD4JulEnergy = m_cf.at(CF_TOD4JulEnergy, 1);
	double year1_TOD4AugEnergy = m_cf.at(CF_TOD4AugEnergy, 1);
	double year1_TOD4SepEnergy = m_cf.at(CF_TOD4SepEnergy, 1);
	double year1_TOD4OctEnergy = m_cf.at(CF_TOD4OctEnergy, 1);
	double year1_TOD4NovEnergy = m_cf.at(CF_TOD4NovEnergy, 1);
	double year1_TOD4DecEnergy = m_cf.at(CF_TOD4DecEnergy, 1);

	double year1_TOD5JanEnergy = m_cf.at(CF_TOD5JanEnergy, 1);
	double year1_TOD5FebEnergy = m_cf.at(CF_TOD5FebEnergy, 1);
	double year1_TOD5MarEnergy = m_cf.at(CF_TOD5MarEnergy, 1);
	double year1_TOD5AprEnergy = m_cf.at(CF_TOD5AprEnergy, 1);
	double year1_TOD5MayEnergy = m_cf.at(CF_TOD5MayEnergy, 1);
	double year1_TOD5JunEnergy = m_cf.at(CF_TOD5JunEnergy, 1);
	double year1_TOD5JulEnergy = m_cf.at(CF_TOD5JulEnergy, 1);
	double year1_TOD5AugEnergy = m_cf.at(CF_TOD5AugEnergy, 1);
	double year1_TOD5SepEnergy = m_cf.at(CF_TOD5SepEnergy, 1);
	double year1_TOD5OctEnergy = m_cf.at(CF_TOD5OctEnergy, 1);
	double year1_TOD5NovEnergy = m_cf.at(CF_TOD5NovEnergy, 1);
	double year1_TOD5DecEnergy = m_cf.at(CF_TOD5DecEnergy, 1);

	double year1_TOD6JanEnergy = m_cf.at(CF_TOD6JanEnergy, 1);
	double year1_TOD6FebEnergy = m_cf.at(CF_TOD6FebEnergy, 1);
	double year1_TOD6MarEnergy = m_cf.at(CF_TOD6MarEnergy, 1);
	double year1_TOD6AprEnergy = m_cf.at(CF_TOD6AprEnergy, 1);
	double year1_TOD6MayEnergy = m_cf.at(CF_TOD6MayEnergy, 1);
	double year1_TOD6JunEnergy = m_cf.at(CF_TOD6JunEnergy, 1);
	double year1_TOD6JulEnergy = m_cf.at(CF_TOD6JulEnergy, 1);
	double year1_TOD6AugEnergy = m_cf.at(CF_TOD6AugEnergy, 1);
	double year1_TOD6SepEnergy = m_cf.at(CF_TOD6SepEnergy, 1);
	double year1_TOD6OctEnergy = m_cf.at(CF_TOD6OctEnergy, 1);
	double year1_TOD6NovEnergy = m_cf.at(CF_TOD6NovEnergy, 1);
	double year1_TOD6DecEnergy = m_cf.at(CF_TOD6DecEnergy, 1);

	double year1_TOD7JanEnergy = m_cf.at(CF_TOD7JanEnergy, 1);
	double year1_TOD7FebEnergy = m_cf.at(CF_TOD7FebEnergy, 1);
	double year1_TOD7MarEnergy = m_cf.at(CF_TOD7MarEnergy, 1);
	double year1_TOD7AprEnergy = m_cf.at(CF_TOD7AprEnergy, 1);
	double year1_TOD7MayEnergy = m_cf.at(CF_TOD7MayEnergy, 1);
	double year1_TOD7JunEnergy = m_cf.at(CF_TOD7JunEnergy, 1);
	double year1_TOD7JulEnergy = m_cf.at(CF_TOD7JulEnergy, 1);
	double year1_TOD7AugEnergy = m_cf.at(CF_TOD7AugEnergy, 1);
	double year1_TOD7SepEnergy = m_cf.at(CF_TOD7SepEnergy, 1);
	double year1_TOD7OctEnergy = m_cf.at(CF_TOD7OctEnergy, 1);
	double year1_TOD7NovEnergy = m_cf.at(CF_TOD7NovEnergy, 1);
	double year1_TOD7DecEnergy = m_cf.at(CF_TOD7DecEnergy, 1);

	double year1_TOD8JanEnergy = m_cf.at(CF_TOD8JanEnergy, 1);
	double year1_TOD8FebEnergy = m_cf.at(CF_TOD8FebEnergy, 1);
	double year1_TOD8MarEnergy = m_cf.at(CF_TOD8MarEnergy, 1);
	double year1_TOD8AprEnergy = m_cf.at(CF_TOD8AprEnergy, 1);
	double year1_TOD8MayEnergy = m_cf.at(CF_TOD8MayEnergy, 1);
	double year1_TOD8JunEnergy = m_cf.at(CF_TOD8JunEnergy, 1);
	double year1_TOD8JulEnergy = m_cf.at(CF_TOD8JulEnergy, 1);
	double year1_TOD8AugEnergy = m_cf.at(CF_TOD8AugEnergy, 1);
	double year1_TOD8SepEnergy = m_cf.at(CF_TOD8SepEnergy, 1);
	double year1_TOD8OctEnergy = m_cf.at(CF_TOD8OctEnergy, 1);
	double year1_TOD8NovEnergy = m_cf.at(CF_TOD8NovEnergy, 1);
	double year1_TOD8DecEnergy = m_cf.at(CF_TOD8DecEnergy, 1);

	double year1_TOD9JanEnergy = m_cf.at(CF_TOD9JanEnergy, 1);
	double year1_TOD9FebEnergy = m_cf.at(CF_TOD9FebEnergy, 1);
	double year1_TOD9MarEnergy = m_cf.at(CF_TOD9MarEnergy, 1);
	double year1_TOD9AprEnergy = m_cf.at(CF_TOD9AprEnergy, 1);
	double year1_TOD9MayEnergy = m_cf.at(CF_TOD9MayEnergy, 1);
	double year1_TOD9JunEnergy = m_cf.at(CF_TOD9JunEnergy, 1);
	double year1_TOD9JulEnergy = m_cf.at(CF_TOD9JulEnergy, 1);
	double year1_TOD9AugEnergy = m_cf.at(CF_TOD9AugEnergy, 1);
	double year1_TOD9SepEnergy = m_cf.at(CF_TOD9SepEnergy, 1);
	double year1_TOD9OctEnergy = m_cf.at(CF_TOD9OctEnergy, 1);
	double year1_TOD9NovEnergy = m_cf.at(CF_TOD9NovEnergy, 1);
	double year1_TOD9DecEnergy = m_cf.at(CF_TOD9DecEnergy, 1);

	for (int y = 2; y <= nyears; y++)
	{
		// compute energy dispatched
		m_cf.at(CF_TODJanEnergy, y) = year1_TODJanEnergy * m_degradation[y]; // *m_cf.at(CF_Availability, y);
		m_cf.at(CF_TODFebEnergy, y) = year1_TODFebEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODMarEnergy, y) = year1_TODMarEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODAprEnergy, y) = year1_TODAprEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODMayEnergy, y) = year1_TODMayEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODJunEnergy, y) = year1_TODJunEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODJulEnergy, y) = year1_TODJulEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODAugEnergy, y) = year1_TODAugEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODSepEnergy, y) = year1_TODSepEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODOctEnergy, y) = year1_TODOctEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODNovEnergy, y) = year1_TODNovEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TODDecEnergy, y) = year1_TODDecEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);

		m_cf.at(CF_TOD1JanEnergy, y) = year1_TOD1JanEnergy * m_degradation[y]; //  * m_cf.at(CF_Availability,y);
		m_cf.at(CF_TOD1FebEnergy, y) = year1_TOD1FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD1MarEnergy, y) = year1_TOD1MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD1AprEnergy, y) = year1_TOD1AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD1MayEnergy, y) = year1_TOD1MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD1JunEnergy, y) = year1_TOD1JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD1JulEnergy, y) = year1_TOD1JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD1AugEnergy, y) = year1_TOD1AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD1SepEnergy, y) = year1_TOD1SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD1OctEnergy, y) = year1_TOD1OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD1NovEnergy, y) = year1_TOD1NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD1DecEnergy, y) = year1_TOD1DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD2JanEnergy, y) = year1_TOD2JanEnergy * m_degradation[y]; //,y);
		m_cf.at(CF_TOD2FebEnergy, y) = year1_TOD2FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD2MarEnergy, y) = year1_TOD2MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD2AprEnergy, y) = year1_TOD2AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD2MayEnergy, y) = year1_TOD2MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD2JunEnergy, y) = year1_TOD2JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD2JulEnergy, y) = year1_TOD2JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD2AugEnergy, y) = year1_TOD2AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD2SepEnergy, y) = year1_TOD2SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD2OctEnergy, y) = year1_TOD2OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD2NovEnergy, y) = year1_TOD2NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD2DecEnergy, y) = year1_TOD2DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD3JanEnergy, y) = year1_TOD3JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD3FebEnergy, y) = year1_TOD3FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD3MarEnergy, y) = year1_TOD3MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD3AprEnergy, y) = year1_TOD3AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD3MayEnergy, y) = year1_TOD3MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD3JunEnergy, y) = year1_TOD3JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD3JulEnergy, y) = year1_TOD3JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD3AugEnergy, y) = year1_TOD3AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD3SepEnergy, y) = year1_TOD3SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD3OctEnergy, y) = year1_TOD3OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD3NovEnergy, y) = year1_TOD3NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD3DecEnergy, y) = year1_TOD3DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD4JanEnergy, y) = year1_TOD4JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD4FebEnergy, y) = year1_TOD4FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD4MarEnergy, y) = year1_TOD4MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD4AprEnergy, y) = year1_TOD4AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD4MayEnergy, y) = year1_TOD4MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD4JunEnergy, y) = year1_TOD4JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD4JulEnergy, y) = year1_TOD4JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD4AugEnergy, y) = year1_TOD4AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD4SepEnergy, y) = year1_TOD4SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD4OctEnergy, y) = year1_TOD4OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD4NovEnergy, y) = year1_TOD4NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD4DecEnergy, y) = year1_TOD4DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD5JanEnergy, y) = year1_TOD5JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD5FebEnergy, y) = year1_TOD5FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD5MarEnergy, y) = year1_TOD5MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD5AprEnergy, y) = year1_TOD5AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD5MayEnergy, y) = year1_TOD5MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD5JunEnergy, y) = year1_TOD5JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD5JulEnergy, y) = year1_TOD5JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD5AugEnergy, y) = year1_TOD5AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD5SepEnergy, y) = year1_TOD5SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD5OctEnergy, y) = year1_TOD5OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD5NovEnergy, y) = year1_TOD5NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD5DecEnergy, y) = year1_TOD5DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD6JanEnergy, y) = year1_TOD6JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD6FebEnergy, y) = year1_TOD6FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD6MarEnergy, y) = year1_TOD6MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD6AprEnergy, y) = year1_TOD6AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD6MayEnergy, y) = year1_TOD6MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD6JunEnergy, y) = year1_TOD6JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD6JulEnergy, y) = year1_TOD6JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD6AugEnergy, y) = year1_TOD6AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD6SepEnergy, y) = year1_TOD6SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD6OctEnergy, y) = year1_TOD6OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD6NovEnergy, y) = year1_TOD6NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD6DecEnergy, y) = year1_TOD6DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD7JanEnergy, y) = year1_TOD7JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD7FebEnergy, y) = year1_TOD7FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD7MarEnergy, y) = year1_TOD7MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD7AprEnergy, y) = year1_TOD7AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD7MayEnergy, y) = year1_TOD7MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD7JunEnergy, y) = year1_TOD7JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD7JulEnergy, y) = year1_TOD7JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD7AugEnergy, y) = year1_TOD7AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD7SepEnergy, y) = year1_TOD7SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD7OctEnergy, y) = year1_TOD7OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD7NovEnergy, y) = year1_TOD7NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD7DecEnergy, y) = year1_TOD7DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD8JanEnergy, y) = year1_TOD8JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD8FebEnergy, y) = year1_TOD8FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD8MarEnergy, y) = year1_TOD8MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD8AprEnergy, y) = year1_TOD8AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD8MayEnergy, y) = year1_TOD8MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD8JunEnergy, y) = year1_TOD8JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD8JulEnergy, y) = year1_TOD8JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD8AugEnergy, y) = year1_TOD8AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD8SepEnergy, y) = year1_TOD8SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD8OctEnergy, y) = year1_TOD8OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD8NovEnergy, y) = year1_TOD8NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD8DecEnergy, y) = year1_TOD8DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD9JanEnergy, y) = year1_TOD9JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD9FebEnergy, y) = year1_TOD9FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD9MarEnergy, y) = year1_TOD9MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD9AprEnergy, y) = year1_TOD9AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD9MayEnergy, y) = year1_TOD9MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD9JunEnergy, y) = year1_TOD9JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD9JulEnergy, y) = year1_TOD9JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD9AugEnergy, y) = year1_TOD9AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD9SepEnergy, y) = year1_TOD9SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD9OctEnergy, y) = year1_TOD9OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD9NovEnergy, y) = year1_TOD9NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD9DecEnergy, y) = year1_TOD9DecEnergy * m_degradation[y];
	}
	return true;
}


bool dispatch_periods::compute_lifetime_dispatch_output(int nyears)
{
	//Calculate energy dispatched in each dispatch period 
	ssc_number_t *hourly_enet; // hourly energy output


	int h;
	size_t count;

	// hourly energy includes all curtailment, availability
	hourly_enet = m_cm->as_array("hourly_energy", &count);
	if ((int)count != (8760 * nyears))
	{
		std::stringstream outm;
		outm << "Bad hourly energy output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760 * nyears << ")";
		m_cm->log(outm.str());
		return false;
	}

	//// hourly dispatch
	//dispatch_periods hourly_dispatch(this);
	//if (!hourly_dispatch.setup())
	//	throw exec_error("ippppa", "failed to setup dispatch periods: " + hourly_dispatch.error());



	for (int y = 1; y <= nyears; y++)
	{
		m_cf.at(CF_TOD1Energy, y) = 0;
		m_cf.at(CF_TOD2Energy, y) = 0;
		m_cf.at(CF_TOD3Energy, y) = 0;
		m_cf.at(CF_TOD4Energy, y) = 0;
		m_cf.at(CF_TOD5Energy, y) = 0;
		m_cf.at(CF_TOD6Energy, y) = 0;
		m_cf.at(CF_TOD7Energy, y) = 0;
		m_cf.at(CF_TOD8Energy, y) = 0;
		m_cf.at(CF_TOD9Energy, y) = 0;

		for (h = 0; h<8760; h++)
		{
			switch (m_periods[h])
			{
			case 1:
				m_cf.at(CF_TOD1Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 2:
				m_cf.at(CF_TOD2Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 3:
				m_cf.at(CF_TOD3Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 4:
				m_cf.at(CF_TOD4Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 5:
				m_cf.at(CF_TOD5Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 6:
				m_cf.at(CF_TOD6Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 7:
				m_cf.at(CF_TOD7Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 8:
				m_cf.at(CF_TOD8Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			case 9:
				m_cf.at(CF_TOD9Energy, y) += hourly_enet[(y - 1) * 8760 + h];
				break;
			}
		}
	}


	return true;
}

bool dispatch_periods::process_lifetime_dispatch_output(int nyears)
{
	//Calculate energy dispatched in each dispatch period 
	ssc_number_t *hourly_enet; // hourly energy output

	size_t count;

	// hourly energy include all curtailment, availability 
	hourly_enet = m_cm->as_array("hourly_energy", &count);
	if ((int)count != (8760 * nyears))
	{
		std::stringstream outm;
		outm << "Bad hourly energy output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760 * nyears << ")";
		m_cm->log(outm.str());
		return false;
	}

	//// hourly dispatch
	//dispatch_periods hourly_dispatch(this);
	//if (!hourly_dispatch.setup())
	//	throw exec_error("ippppa", "failed to setup dispatch periods: " + hourly_dispatch.error());


	for (int y = 1; y <= nyears; y++)
	{
		m_cf.at(CF_TODJanEnergy, y) = 0;
		m_cf.at(CF_TODFebEnergy, y) = 0;
		m_cf.at(CF_TODMarEnergy, y) = 0;
		m_cf.at(CF_TODAprEnergy, y) = 0;
		m_cf.at(CF_TODMayEnergy, y) = 0;
		m_cf.at(CF_TODJunEnergy, y) = 0;
		m_cf.at(CF_TODJulEnergy, y) = 0;
		m_cf.at(CF_TODAugEnergy, y) = 0;
		m_cf.at(CF_TODSepEnergy, y) = 0;
		m_cf.at(CF_TODOctEnergy, y) = 0;
		m_cf.at(CF_TODNovEnergy, y) = 0;
		m_cf.at(CF_TODDecEnergy, y) = 0;

		m_cf.at(CF_TOD1JanEnergy, y) = 0;
		m_cf.at(CF_TOD1FebEnergy, y) = 0;
		m_cf.at(CF_TOD1MarEnergy, y) = 0;
		m_cf.at(CF_TOD1AprEnergy, y) = 0;
		m_cf.at(CF_TOD1MayEnergy, y) = 0;
		m_cf.at(CF_TOD1JunEnergy, y) = 0;
		m_cf.at(CF_TOD1JulEnergy, y) = 0;
		m_cf.at(CF_TOD1AugEnergy, y) = 0;
		m_cf.at(CF_TOD1SepEnergy, y) = 0;
		m_cf.at(CF_TOD1OctEnergy, y) = 0;
		m_cf.at(CF_TOD1NovEnergy, y) = 0;
		m_cf.at(CF_TOD1DecEnergy, y) = 0;

		m_cf.at(CF_TOD2JanEnergy, y) = 0;
		m_cf.at(CF_TOD2FebEnergy, y) = 0;
		m_cf.at(CF_TOD2MarEnergy, y) = 0;
		m_cf.at(CF_TOD2AprEnergy, y) = 0;
		m_cf.at(CF_TOD2MayEnergy, y) = 0;
		m_cf.at(CF_TOD2JunEnergy, y) = 0;
		m_cf.at(CF_TOD2JulEnergy, y) = 0;
		m_cf.at(CF_TOD2AugEnergy, y) = 0;
		m_cf.at(CF_TOD2SepEnergy, y) = 0;
		m_cf.at(CF_TOD2OctEnergy, y) = 0;
		m_cf.at(CF_TOD2NovEnergy, y) = 0;
		m_cf.at(CF_TOD2DecEnergy, y) = 0;

		m_cf.at(CF_TOD3JanEnergy, y) = 0;
		m_cf.at(CF_TOD3FebEnergy, y) = 0;
		m_cf.at(CF_TOD3MarEnergy, y) = 0;
		m_cf.at(CF_TOD3AprEnergy, y) = 0;
		m_cf.at(CF_TOD3MayEnergy, y) = 0;
		m_cf.at(CF_TOD3JunEnergy, y) = 0;
		m_cf.at(CF_TOD3JulEnergy, y) = 0;
		m_cf.at(CF_TOD3AugEnergy, y) = 0;
		m_cf.at(CF_TOD3SepEnergy, y) = 0;
		m_cf.at(CF_TOD3OctEnergy, y) = 0;
		m_cf.at(CF_TOD3NovEnergy, y) = 0;
		m_cf.at(CF_TOD3DecEnergy, y) = 0;

		m_cf.at(CF_TOD4JanEnergy, y) = 0;
		m_cf.at(CF_TOD4FebEnergy, y) = 0;
		m_cf.at(CF_TOD4MarEnergy, y) = 0;
		m_cf.at(CF_TOD4AprEnergy, y) = 0;
		m_cf.at(CF_TOD4MayEnergy, y) = 0;
		m_cf.at(CF_TOD4JunEnergy, y) = 0;
		m_cf.at(CF_TOD4JulEnergy, y) = 0;
		m_cf.at(CF_TOD4AugEnergy, y) = 0;
		m_cf.at(CF_TOD4SepEnergy, y) = 0;
		m_cf.at(CF_TOD4OctEnergy, y) = 0;
		m_cf.at(CF_TOD4NovEnergy, y) = 0;
		m_cf.at(CF_TOD4DecEnergy, y) = 0;

		m_cf.at(CF_TOD5JanEnergy, y) = 0;
		m_cf.at(CF_TOD5FebEnergy, y) = 0;
		m_cf.at(CF_TOD5MarEnergy, y) = 0;
		m_cf.at(CF_TOD5AprEnergy, y) = 0;
		m_cf.at(CF_TOD5MayEnergy, y) = 0;
		m_cf.at(CF_TOD5JunEnergy, y) = 0;
		m_cf.at(CF_TOD5JulEnergy, y) = 0;
		m_cf.at(CF_TOD5AugEnergy, y) = 0;
		m_cf.at(CF_TOD5SepEnergy, y) = 0;
		m_cf.at(CF_TOD5OctEnergy, y) = 0;
		m_cf.at(CF_TOD5NovEnergy, y) = 0;
		m_cf.at(CF_TOD5DecEnergy, y) = 0;

		m_cf.at(CF_TOD6JanEnergy, y) = 0;
		m_cf.at(CF_TOD6FebEnergy, y) = 0;
		m_cf.at(CF_TOD6MarEnergy, y) = 0;
		m_cf.at(CF_TOD6AprEnergy, y) = 0;
		m_cf.at(CF_TOD6MayEnergy, y) = 0;
		m_cf.at(CF_TOD6JunEnergy, y) = 0;
		m_cf.at(CF_TOD6JulEnergy, y) = 0;
		m_cf.at(CF_TOD6AugEnergy, y) = 0;
		m_cf.at(CF_TOD6SepEnergy, y) = 0;
		m_cf.at(CF_TOD6OctEnergy, y) = 0;
		m_cf.at(CF_TOD6NovEnergy, y) = 0;
		m_cf.at(CF_TOD6DecEnergy, y) = 0;

		m_cf.at(CF_TOD7JanEnergy, y) = 0;
		m_cf.at(CF_TOD7FebEnergy, y) = 0;
		m_cf.at(CF_TOD7MarEnergy, y) = 0;
		m_cf.at(CF_TOD7AprEnergy, y) = 0;
		m_cf.at(CF_TOD7MayEnergy, y) = 0;
		m_cf.at(CF_TOD7JunEnergy, y) = 0;
		m_cf.at(CF_TOD7JulEnergy, y) = 0;
		m_cf.at(CF_TOD7AugEnergy, y) = 0;
		m_cf.at(CF_TOD7SepEnergy, y) = 0;
		m_cf.at(CF_TOD7OctEnergy, y) = 0;
		m_cf.at(CF_TOD7NovEnergy, y) = 0;
		m_cf.at(CF_TOD7DecEnergy, y) = 0;

		m_cf.at(CF_TOD8JanEnergy, y) = 0;
		m_cf.at(CF_TOD8FebEnergy, y) = 0;
		m_cf.at(CF_TOD8MarEnergy, y) = 0;
		m_cf.at(CF_TOD8AprEnergy, y) = 0;
		m_cf.at(CF_TOD8MayEnergy, y) = 0;
		m_cf.at(CF_TOD8JunEnergy, y) = 0;
		m_cf.at(CF_TOD8JulEnergy, y) = 0;
		m_cf.at(CF_TOD8AugEnergy, y) = 0;
		m_cf.at(CF_TOD8SepEnergy, y) = 0;
		m_cf.at(CF_TOD8OctEnergy, y) = 0;
		m_cf.at(CF_TOD8NovEnergy, y) = 0;
		m_cf.at(CF_TOD8DecEnergy, y) = 0;

		m_cf.at(CF_TOD9JanEnergy, y) = 0;
		m_cf.at(CF_TOD9FebEnergy, y) = 0;
		m_cf.at(CF_TOD9MarEnergy, y) = 0;
		m_cf.at(CF_TOD9AprEnergy, y) = 0;
		m_cf.at(CF_TOD9MayEnergy, y) = 0;
		m_cf.at(CF_TOD9JunEnergy, y) = 0;
		m_cf.at(CF_TOD9JulEnergy, y) = 0;
		m_cf.at(CF_TOD9AugEnergy, y) = 0;
		m_cf.at(CF_TOD9SepEnergy, y) = 0;
		m_cf.at(CF_TOD9OctEnergy, y) = 0;
		m_cf.at(CF_TOD9NovEnergy, y) = 0;
		m_cf.at(CF_TOD9DecEnergy, y) = 0;

		int i = 0;
		for (int m = 0; m<12; m++)
		{
			for (int d = 0; d<util::nday[m]; d++)
			{
				for (int h = 0; h<24 && i<8760 && m * 24 + h<288; h++)
				{
					switch (m)
					{
					case 0:
						m_cf.at(CF_TODJanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JanEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 1:
						m_cf.at(CF_TODFebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9FebEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 2:
						m_cf.at(CF_TODMarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9MarEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 3:
						m_cf.at(CF_TODAprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9AprEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 4:
						m_cf.at(CF_TODMayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9MayEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 5:
						m_cf.at(CF_TODJunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JunEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 6:
						m_cf.at(CF_TODJulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JulEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 7:
						m_cf.at(CF_TODAugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9AugEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 8:
						m_cf.at(CF_TODSepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9SepEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 9:
						m_cf.at(CF_TODOctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9OctEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 10:
						m_cf.at(CF_TODNovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9NovEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 11:
						m_cf.at(CF_TODDecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9DecEnergy, y) += hourly_enet[(y - 1) * 8760 + i];
							break;
						}
						break;
					}
					i++;
				}
			}
		}

	}

	return true;
}
