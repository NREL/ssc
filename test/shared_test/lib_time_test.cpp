#include "lib_time_test.h"
#include "lib_time.h"

// Single year is 60 min, lifetime is 30 min, 25 years
TEST_F(libTimeTests, TestLifetimeInterpolation)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear60min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single[0], 50);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}
TEST_F(libTimeTests, TestSameSizeSingleYear)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear60min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single[0], 100);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}

TEST_F(libTimeTests, TestSameSizeSubhourlySingleYear)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single[0], 100);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}

TEST_F(libTimeTests, TestSameSizeSubhourlyLifetime)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime30min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * 2 * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year * 2);
	EXPECT_EQ(dt_hour, 0.5);
	EXPECT_EQ(lifetime_from_single[0], 100);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}


// Test downsample
TEST_F(libTimeTests, TestLifetimeDownsample)
{
	is_lifetime = true;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = lifetime60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year * n_years);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single[0], 100 * 2);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}

// Test downsample
TEST_F(libTimeTests, TestSingleyearDownsample)
{
	is_lifetime = false;
	std::vector<float> lifetime_from_single;
	size_t n_rec_lifetime = singleyear60min.size();
	size_t n_rec_singleyear;
	double dt_hour;
	single_year_to_lifetime_interpolated<float>(is_lifetime, n_years, n_rec_lifetime,
		singleyear30min, lifetime_from_single, n_rec_singleyear, dt_hour);

	EXPECT_EQ(n_rec_lifetime, util::hours_per_year);
	EXPECT_EQ(n_rec_singleyear, util::hours_per_year);
	EXPECT_EQ(dt_hour, 1.0);
	EXPECT_EQ(lifetime_from_single[0], 100 * 2);
	EXPECT_EQ(lifetime_from_single.size(), n_rec_lifetime);
}

