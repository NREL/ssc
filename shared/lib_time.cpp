#include "lib_time.h"
#include "lib_util.h"

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
	if (singleyear_vector.size() > 0)
	{
		// Interpolate single year vector to dt_hour
		std::vector<T> singleyear_sampled;
		if (singleyear_vector.size() <= n_rec_single_year) {
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour_singleyear_input; sy++) {
					for (size_t i = 0; i < (size_t)interpolation_factor; i++) {
						singleyear_sampled.push_back(singleyear_vector[sy] / interpolation_factor);
					}
				}
			}
		}
		// Downsample single year vector to dt_hour
		else {
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour; sy++) {
					singleyear_sampled.push_back(singleyear_vector[sy] / interpolation_factor);
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
	else {
		for (size_t i = 0; i < n_rec_lifetime; i++) {
			lifetime_from_singleyear_vector.push_back(0);
		}
	}
}

template void single_year_to_lifetime_interpolated<double>(bool, size_t, size_t,std::vector<double>, std::vector<double> &, size_t &, double &);
template void single_year_to_lifetime_interpolated<float>(bool, size_t, size_t, std::vector<float>, std::vector<float> &, size_t &, double &);
