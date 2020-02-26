#include <vector>
#include <string>
#include <gtest/gtest.h>

#include "../ssc/cmod_utilityrate5_eqns.h"
#include "vartab.h"

TEST(URDBv7_cmod_utilityrate5_eqns, ElectricityRates_format_as_URDBv7) {
    auto data = new var_table;

    ssc_number_t p_ur_ec_sched_weekday[288] = {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4};
    ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
    ssc_number_t p_ur_ec_sched_weekend[288] = {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                                               4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4};
    ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
    ssc_number_t p_ur_ec_tou_mat[24] = {2, 2, 9.9999996802856925e+37, 0, 0.069078996777534485, 0,
                                        2, 1, 100, 0, 0.056908998638391495, 0,
                                        1, 2, 9.9999996802856925e+37, 0, 0.082948997616767883, 0,
                                        1, 1, 100, 0, 0.070768997073173523, 0};
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_data_set_number(data, "ur_metering_option", 0);
    ssc_data_set_number(data, "ur_monthly_fixed_charge", 35.28);
    ssc_data_set_number(data, "ur_monthly_min_charge", 1);
    ssc_data_set_number(data, "ur_annual_min_charge", 12);
    ssc_number_t p_ur_dc_sched_weekday[288] = {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2};
    ssc_data_set_matrix(data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24);
    ssc_number_t p_ur_dc_sched_weekend[288] = {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2};
    ssc_data_set_matrix(data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24);
    ssc_number_t p_ur_dc_tou_mat[16] = {1, 1, 100, 19.538999557495117,
                                        1, 2, 9.9999996802856925e+37, 13.093000411987305,
                                        2, 1, 100, 8.0909996032714844,
                                        2, 2, 9.9999996802856925e+37, 4.6760001182556152};
    ssc_data_set_matrix(data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 4, 4);
    ssc_number_t p_ur_dc_flat_mat[48] = {0, 1, 9.9999996802856925e+37, 0, 1, 1, 9.9999996802856925e+37, 0, 2, 1,
                                         9.9999996802856925e+37, 0, 3, 1, 9.9999996802856925e+37, 0, 4, 1,
                                         9.9999996802856925e+37, 0, 5, 1, 9.9999996802856925e+37, 0, 6, 1,
                                         9.9999996802856925e+37, 0, 7, 1, 9.9999996802856925e+37, 0, 8, 1,
                                         9.9999996802856925e+37, 0, 9, 1, 9.9999996802856925e+37, 0, 10, 1,
                                         9.9999996802856925e+37, 0, 11, 1, 9.9999996802856925e+37, 0};
    ssc_data_set_matrix(data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4);

    ElectricityRates_format_as_URDBv7(data);

    auto urdb_data = data->lookup("urdb_data")->table;
    auto rules = urdb_data.lookup("dgrules")->str;
    EXPECT_STRCASEEQ(rules.c_str(), "Net Metering");

    auto monthly_fixed = urdb_data.lookup("fixedmonthlycharge")->num;
    EXPECT_NEAR(monthly_fixed, 35.28, 1e-3);

    auto min_charge = urdb_data.lookup("minmonthlycharge")->num;
    EXPECT_NEAR(min_charge, 1, 1e-3);

    min_charge = urdb_data.lookup("annualmincharge")->num;
    EXPECT_NEAR(min_charge, 12, 1e-3);

    auto ec_wd_sched = urdb_data.lookup("energyweekdayschedule")->num.data();
    auto ec_we_sched = urdb_data.lookup("energyweekendschedule")->num.data();
    auto dc_wd_sched = urdb_data.lookup("demandweekdayschedule")->num.data();
    auto dc_we_sched = urdb_data.lookup("demandweekendschedule")->num.data();
    for (size_t i = 0; i < 12 * 24; i++) {
        EXPECT_EQ(ec_wd_sched[i], p_ur_ec_sched_weekday[i] - 1);
        EXPECT_EQ(ec_we_sched[i], p_ur_ec_sched_weekend[i] - 1);
        EXPECT_EQ(dc_wd_sched[i], p_ur_dc_sched_weekday[i] - 1);
        EXPECT_EQ(dc_we_sched[i], p_ur_dc_sched_weekend[i] - 1);
    }

    auto dc_flat = urdb_data.lookup("flatdemandstructure")->arr_vector();
    for (size_t i = 0; i < 12; i++) {
        EXPECT_NEAR(dc_flat[i], p_ur_dc_flat_mat[i * 4 + 3], 1e-3);
    }

    auto ec_tou_mat = urdb_data.lookup("energyratestructure")->mat;
    auto period = ec_tou_mat[0];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 0.070, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 0.083, 1e-3);
    period = ec_tou_mat[1];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 0.057, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 0.069, 1e-3);

    auto dc_tou_mat = urdb_data.lookup("demandratestructure")->mat;
    period = dc_tou_mat[0];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 19.539, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 13.093, 1e-3);
    period = dc_tou_mat[1];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 8.09, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 4.676, 1e-3);

//    delete data;
}
