#include "lib_time_test.h"
#include "lib_time.h"


TEST_F(libTimeTests, TestInterpolation)
{
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime;
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, lifetime30min,
		singleyear60min, lifetime_from_single, n_rec_lifetime, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single[0], 25);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}