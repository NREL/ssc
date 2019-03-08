#include "lib_time.h"
#include "lib_util.h"

template <class T>
void single_year_to_lifetime_interpolated(
	bool is_lifetime,
	size_t n_years,
	std::vector<T> lifetime_vector,
	std::vector<T> singleyear_vector,
	std::vector<T> &lifetime_from_singleyear_vector,
	size_t &n_rec_lifetime,
	size_t &n_rec_single_year,
	double &dt_hour)
{
	n_rec_lifetime = lifetime_vector.size();
	n_rec_single_year = n_rec_lifetime;

	dt_hour = (double)util::hours_per_year / (double)lifetime_vector.size();
	if (is_lifetime) {
		dt_hour = (double)(util::hours_per_year * n_years) / (double)(lifetime_vector.size());
		n_rec_single_year = n_rec_lifetime / n_years;
	}
	else {
		n_years = 1;
	}
	size_t step_per_hour = (size_t)(1 / dt_hour);

	lifetime_from_singleyear_vector.reserve(n_rec_lifetime);

	if (singleyear_vector.size() > 0)
	{
		double dt_hour_singleyear = (double)util::hours_per_year / (double)singleyear_vector.size();
		if (dt_hour_singleyear != dt_hour || is_lifetime)
		{
			// Interpolate singleyear and resize for multi-year
			for (size_t iyear = 0; iyear < n_years; iyear++) {
				for (size_t hour = 0; hour < util::hours_per_year; hour++) {
					for (size_t jj = 0; jj < step_per_hour; jj++) {
						lifetime_from_singleyear_vector.push_back(singleyear_vector[hour] / (T)step_per_hour);
					}
				}
			}
		}
	}
	else {
		for (size_t i = 0; i < n_rec_lifetime; i++) {
			lifetime_from_singleyear_vector.push_back(0);
		}
	}
}

template void single_year_to_lifetime_interpolated<double>(bool, size_t,std::vector<double>,std::vector<double>, std::vector<double> &, size_t &, size_t &, double &);
template void single_year_to_lifetime_interpolated<float>(bool, size_t, std::vector<float>, std::vector<float>, std::vector<float> &, size_t &, size_t &, double &);
