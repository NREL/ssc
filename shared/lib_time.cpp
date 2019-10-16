#include "lib_time.h"

/**
*  \function  single_year_to_lifetime_interpolated 
*  
*  Takes information about the desired lifetime vector, a single-year vector, and returns the single-year vector
*  as a lifetime length vector, interpolated as needed.  As an example, consider that solar generation is passed in
*  as a 15-minute, 25 year vector, and the electric load is currently a single-year and hourly.  The function will
*  take the single-year hourly load, interpolate it to 15-minutes, and scale to 25 years.
*
* \param[in] is_lifetime (true/false)
* \param[in] n_years (1 - 100)
* \param[in] n_lifetime (length of desired lifetime vector)
* \param[in] singleyear_vector (the single year vector to scale to lifetime and interpolate)
* \param[out] lifetime_from_singleyear_vector (the lifetime, interpolated vector)
* \param[out] n_rec_single_year (the length of a single year vector, interpolated at the lifetime vector timescale)
* \param[out] dt_hour (the time step in hours)
*/
template <class T>
void single_year_to_lifetime_interpolated(
	bool is_lifetime,
	size_t n_years,
	size_t n_rec_lifetime,
	std::vector<T> singleyear_vector,
	std::vector<T> &lifetime_from_singleyear_vector,
	size_t &n_rec_single_year,
	double &dt_hour)
{
	// Parse lifetime properties
	n_rec_single_year = n_rec_lifetime;
	dt_hour = (double)(util::hours_per_year) / (double)(n_rec_lifetime);

	if (is_lifetime) {
		dt_hour = (double)(util::hours_per_year * n_years) / n_rec_lifetime;
		n_rec_single_year = n_rec_lifetime / n_years;
	}
	else {
		n_years = 1;
	}
	size_t step_per_hour = (size_t)(1 / dt_hour);
	lifetime_from_singleyear_vector.reserve(n_rec_lifetime);

	// Parse single year properties
	double dt_hour_singleyear_input = (double)(util::hours_per_year) / (double)(singleyear_vector.size());
	size_t step_per_hour_singleyear_input = (size_t)(1 / dt_hour_singleyear_input);
	T interpolation_factor = (T)step_per_hour / (T)step_per_hour_singleyear_input;

	// Possible that there is no single year vector
	if (singleyear_vector.size() > 1)
	{
		// Interpolate single year vector to dt_hour
		std::vector<T> singleyear_sampled;
		if (singleyear_vector.size() <= n_rec_single_year) {
			size_t sy_idx = 0;
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour_singleyear_input; sy++) {
					for (size_t i = 0; i < (size_t)interpolation_factor; i++) {
						singleyear_sampled.push_back(singleyear_vector[sy_idx] / interpolation_factor);
					}
					sy_idx++;
				}
			}
		}
		// Downsample single year vector to dt_hour
		else {
			size_t sy_idx = 0;
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour; sy++) {
					// eventually add more sophisticated downsampling, ignoring information
					singleyear_sampled.push_back(singleyear_vector[(size_t)(sy_idx/interpolation_factor)] / interpolation_factor);
					sy_idx++;
				}
			}
		}

		// Scale single year interpolated vector to lifetime
		for (size_t y = 0; y < n_years; y++) {
			for (size_t i = 0; i < n_rec_single_year; i++) {
				lifetime_from_singleyear_vector.push_back(singleyear_sampled[i]);
			}
		}
	}
	// In the case of no single year vector, create lifetime vector of zeros
	else if (singleyear_vector.empty() ) {
	    for (size_t i = 0; i < n_rec_lifetime; i++)
		    lifetime_from_singleyear_vector.emplace_back(0);
	}
	else if (singleyear_vector.size() == 1) {
	    for (size_t i = 0; i < n_rec_lifetime; i++)
	        lifetime_from_singleyear_vector.emplace_back(lifetime_from_singleyear_vector[0]);
	}
}

template void single_year_to_lifetime_interpolated<double>(bool, size_t, size_t,std::vector<double>, std::vector<double> &, size_t &, double &);
template void single_year_to_lifetime_interpolated<float>(bool, size_t, size_t, std::vector<float>, std::vector<float> &, size_t &, double &);



/**
*  \function  flatten_diurnal
*
* Function takes in a weekday and weekend schedule, plus the period values and an optional multiplier and returns
* a vector of the scaled hourly values throughout the entire year
*
* \param[in] weekday_schedule - 12x24 scheduled of periods
* \param[in] weekday_schedule - 12x24 scheduled of periods
* \param[in] steps_per_hour - Number of time steps per hour
* \param[in] period_values - the value assigned to each period number
* \param[in] multiplier - a multiplier on the period value
* \param[out] flat_vector - The 8760 values at each hour 
*/
template <class T>
std::vector<T> flatten_diurnal(util::matrix_t<size_t> weekday_schedule, util::matrix_t<size_t> weekend_schedule, size_t steps_per_hour, std::vector<T> period_values, T multiplier)
{
	std::vector<T> flat_vector;
	flat_vector.reserve(8760 * steps_per_hour);
	size_t month, hour, iprofile;
	T period_value;

	for (size_t hour_of_year = 0; hour_of_year != 8760; hour_of_year++)
	{
		util::month_hour(hour_of_year % 8760, month, hour);
		if (util::weekday(hour_of_year))
			iprofile = weekday_schedule(month - 1, hour - 1);
		else
			iprofile = weekend_schedule(month - 1, hour - 1);

		period_value = period_values[iprofile - 1];
		for (size_t s = 0; s < steps_per_hour; s++) {
			flat_vector.push_back(period_value * multiplier);
		}
	}
	return flat_vector;
}

template std::vector<double> flatten_diurnal(util::matrix_t<size_t> weekday_schedule, util::matrix_t<size_t> weekend_schedule, size_t steps_per_hour, std::vector<double> period_values, double multiplier);
