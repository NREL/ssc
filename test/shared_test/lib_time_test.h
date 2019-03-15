#ifndef __LIB_TIME_TEST_H__
#define __LIB_TIME_TEST_H__

#include "lib_util.h"
#include <gtest/gtest.h>
#include <vector>

class libTimeTests : public ::testing::Test
{
protected:

	bool is_lifetime;
	size_t n_years;
	std::vector<float> lifetime60min;
	std::vector<float> lifetime30min;
	std::vector<float> singleyear60min;


	void SetUp()
	{
		is_lifetime = true;
		n_years = 25;
		
		lifetime60min.reserve(n_years * util::hours_per_year);
		for (size_t i = 0; i < n_years * util::hours_per_year; i++) {
			lifetime60min.push_back(100);
		}

		singleyear60min.reserve(util::hours_per_year);
		for (size_t i = 0; i <  util::hours_per_year; i++) {
			singleyear60min.push_back(50);
		}

		lifetime30min.reserve(2 * n_years * util::hours_per_year);
		for (size_t i = 0; i < n_years * 2 * util::hours_per_year; i++) {
			lifetime30min.push_back(100);
		}
	}
};


#endif // !__LIB_TIME_TEST_H__
