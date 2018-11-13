#include <gtest/gtest.h>
#include <lib_util.h>
#include <string>


TEST(libUtilTests, testFormat)
{
	// test single input
	std::string str = "invalid number of data records (43): must be an integer multiple of 8760";
	ASSERT_EQ(util::format("invalid number of data records (%d): must be an integer multiple of 8760", 43), str);

	// test multiple inputs
	str = "query point (301.3, 10.4) is too far out of convex hull of data (dist=4.3)... estimating value from 5 parameter modele at (2.2, 2.1)=2.4";
	ASSERT_EQ(util::format("query point (%lg, %lg) is too far out of convex hull of data (dist=%lg)... estimating value from 5 parameter modele at (%lg, %lg)=%lg",
		301.3, 10.4, 4.3, 2.2, 2.1, 2.4), str);
}