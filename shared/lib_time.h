#ifndef __LIB_TIME_H__
#define __LIB_TIME_H__

#include <vector>
#include <cstddef>
/**
Function takes lifetime (multi-year) vector and single year vector of possibly lower time resolution
and returns the single year vector scaled to lifetime at the time resolution of the lifetime input vector
*/
template <typename T>
void single_year_to_lifetime_interpolated(
	bool is_lifetime,
	size_t n_years,
	size_t n_lifetime,
	std::vector<T> singleyear_vector,
	std::vector<T> &lifetime_from_singleyear_vector,
	size_t &n_rec_single_year,
	double &dt_hour);


#endif // !__LIB_TIME_H__

