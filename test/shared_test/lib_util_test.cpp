#include <string>
#include <gtest/gtest.h>
#include <lib_util.h>
#include <vartab.h>


TEST(libUtilTests, testFormat_lib_util)
{
	// test single input
	std::string str = "invalid number of data records (43): must be an integer multiple of 8760";
	ASSERT_EQ(util::format("invalid number of data records (%d): must be an integer multiple of 8760", 43), str);

	// test multiple inputs
	str = "query point (301.3, 10.4) is too far out of convex hull of data (dist=4.3)... estimating value from 5 parameter modele at (2.2, 2.1)=2.4";
	ASSERT_EQ(util::format("query point (%lg, %lg) is too far out of convex hull of data (dist=%lg)... estimating value from 5 parameter modele at (%lg, %lg)=%lg",
		301.3, 10.4, 4.3, 2.2, 2.1, 2.4), str);
}

TEST(sscapiTest, SSC_DATARR_test)
{
    std::vector<ssc_var_t > vd;
    vd.resize(2);
    for (auto& i : vd){
        i = ssc_var_create();
        ssc_var_set_number(i, 2);
    }

    auto data = ssc_data_create();

    ssc_data_set_data_array(data, "array", &vd[0], 2);
    int n;
    auto data_arr = static_cast<var_data*>(ssc_data_get_data_array(data, "array", &n));
    for (size_t i = 0; i < n; i++){
        double var = ssc_var_get_number(static_cast<ssc_var_t>(&data_arr[i]));
        assert(var == 2);
    }
}

TEST(sscapiTest, SSC_DATMAT_test)
{
    std::vector<ssc_var_t > vd;
    vd.resize(2);
    for (auto& i : vd){
        i = ssc_var_create();
        ssc_var_set_number(i, 2);
    }
    std::vector<std::vector<ssc_var_t>> vm;
    vm.push_back(vd);
    vm.push_back(vd);

    auto data = ssc_data_create();

    ssc_data_set_data_matrix(data, "matrix", &vm[0][0], 2, 2);
    int n, m;
    auto data_mat = static_cast<var_data*>(ssc_data_get_data_matrix(data, "matrix", &n, &m));
    for (size_t i = 0; i < n*m; i++){
        double var = ssc_var_get_number(static_cast<ssc_var_t>(&data_mat[i]));
        assert(var == 2);
    }
}