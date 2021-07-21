#include <gtest/gtest.h>
#include <cmath>
#include <json/json.h>
#include <fstream>
#include <algorithm>

#include "lib_util.h"
#include "lib_battery_lifetime_nmc.h"

class lib_battery_lifetime_nmc_test : public ::testing::Test{
protected:
    std::unique_ptr<lifetime_nmc_t> model;

    double dt_hour = 1;
public:
    void SetUp() override {
        model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));
    }
};


TEST_F(lib_battery_lifetime_nmc_test, InitTest) {
    double tol = 0.001;

    //check lifetime_nmc_state_initialization
    auto lifetime_state = model->get_state();
    EXPECT_NEAR(lifetime_state.nmc_li_neg->q_relative_neg, 100, tol);
    EXPECT_NEAR(lifetime_state.nmc_li_neg->q_relative_li, 100, tol);
    EXPECT_EQ(model->get_state().day_age_of_battery, 0);
    EXPECT_EQ(model->get_state().n_cycles, 0);

    //check U_neg, and Voc functions (SOC as a fractional input)
    EXPECT_NEAR(model->calculate_Uneg(0), 1.2868, tol);
    EXPECT_NEAR(model->calculate_Voc(0), 3, tol);
    EXPECT_NEAR(model->calculate_Uneg(0.1), 0.242, tol);
    EXPECT_NEAR(model->calculate_Voc(0.1), 3.4679, tol);
    EXPECT_NEAR(model->calculate_Uneg(0.5), 0.123, tol);
    EXPECT_NEAR(model->calculate_Voc(0.5), 3.6876, tol);
    EXPECT_NEAR(model->calculate_Uneg(0.9), 0.0876, tol);
    EXPECT_NEAR(model->calculate_Voc(0.9), 4.0668, tol);
    EXPECT_NEAR(model->calculate_Uneg(1), 0.0859, tol);
    EXPECT_NEAR(model->calculate_Voc(1), 4.193, tol);
}

TEST_F(lib_battery_lifetime_nmc_test, CopyTest) {
    double tol = 0.001;

    // check lifetime_nmc_state get & set
    auto state = model->get_state();
    state.cycle->cycle_DOD_range = {0, 1};
    state.cycle->cycle_DOD_max = {2, 3};
    model->set_state(state);

    state = model->get_state();
    EXPECT_EQ(state.cycle->cycle_DOD_range[0], 0);
    EXPECT_EQ(state.cycle->cycle_DOD_range[1], 1);
    EXPECT_EQ(state.cycle->cycle_DOD_max[0], 2);
    EXPECT_EQ(state.cycle->cycle_DOD_max[1], 3);

    auto new_model = lifetime_nmc_t(*model);
    state = new_model.get_state();
    EXPECT_EQ(state.cycle->cycle_DOD_range[0], 0);
    EXPECT_EQ(state.cycle->cycle_DOD_range[1], 1);
    EXPECT_EQ(state.cycle->cycle_DOD_max[0], 2);
    EXPECT_EQ(state.cycle->cycle_DOD_max[1], 3);
}

/// run at different days
TEST_F(lib_battery_lifetime_nmc_test, StorageDays) {
    std::vector<double> days = {0, 10, 50 , 500, 5000};
    std::vector<double> expected_q_li = {106.50, 104.36, 103.97, 102.835, 99.66};

    for (size_t i = 0; i < days.back() + 1; i++) {
        for (size_t h = 0; h < 24; h++) {
            size_t hr = i * 24 + h;
            model->runLifetimeModels(hr, false, 50, 50, 25);
        }
        auto pos = std::find(days.begin(), days.end(), (double)i);
        if (pos != days.end()) {
            auto state = model->get_state();
            EXPECT_NEAR(state.nmc_li_neg->q_relative_li, expected_q_li[pos - days.begin()], 0.5);
        }
    }
    EXPECT_EQ((size_t)model->get_state().day_age_of_battery, 5001);
}

/// Run with minute timestep instead
TEST_F(lib_battery_lifetime_nmc_test, StorageMinuteTimestep) {
    double dt_hr = 1. / 60;
    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hr));

    std::vector<double> days = {0, 10, 50 , 500, 5000};
    std::vector<double> expected_q_li = {106.50, 104.36, 103.97, 102.835, 99.66};

    auto steps_per_day = (size_t)(24 / dt_hr);
    for (size_t i = 0; i < days.back() + 1; i++) {
        for (size_t h = 0; h < steps_per_day; h++) {
            size_t hr = i * steps_per_day + h;
            model->runLifetimeModels(hr, false, 50, 50, 25);
        }
        auto pos = std::find(days.begin(), days.end(), i);
        if (pos != days.end()) {
            auto state = model->get_state();
            EXPECT_NEAR(state.nmc_li_neg->q_relative_li, expected_q_li[pos - days.begin()], 0.5);
        }
    }
    EXPECT_EQ((size_t)model->get_state().day_age_of_battery, 5001);
}

/// run at different days at different temperatures
TEST_F(lib_battery_lifetime_nmc_test, StorageTemp) {
    std::vector<double> temps = {0, 10, 15, 40};
    std::vector<double> expected_q_li = {81.73, 93.08, 97.43, 93};

    for (size_t n = 3; n < temps.size(); n++) {
        model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));
        for (size_t d = 0; d < 5000 + 1; d++) {
            for (size_t h = 0; h < 24; h++) {
                size_t hr = d * 24 + h;
                model->runLifetimeModels(hr, false, 50, 50, temps[n]);
            }
        }
        auto state = model->get_state();
        EXPECT_NEAR((size_t)state.nmc_li_neg->q_relative_li, expected_q_li[n], 1);
    }
}

TEST_F(lib_battery_lifetime_nmc_test, StorageTempSmallDt) {
    std::vector<double> temps = { 0, 10, 15, 40 };
    std::vector<double> expected_q_li = { 81.73, 93.08, 97.43, 90 };

    dt_hour = 1 / 60.0 / 60.0 * 10.0;

    for (size_t n = 3; n < temps.size(); n++) {
        model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));
        for (size_t d = 0; d < 5000 + 1; d++) {
            for (size_t h = 0; h < 24; h++) {
                size_t hr = d * 24 + h;
                for (size_t s = 0; s < 600; s++) {
                    model->runLifetimeModels(hr, false, 50, 50, temps[n]);
                }
            }
        }
        auto state = model->get_state();
        EXPECT_NEAR((size_t)state.nmc_li_neg->q_relative_li, expected_q_li[n], 1);
    }
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingHighDOD) {
    size_t day = 0;
    double T = 25.15;
    while (day < 87) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 101.19, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.6, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 94.09, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 99.30, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);

    while (day < 8700) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 8699);
    EXPECT_EQ(state.cycle_range, 80);
    EXPECT_EQ(state.cycle_DOD, 90);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 63.42, 3);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 84.43, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 8700, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingHighTemp) {
    size_t day = 0;
    double T = 35.;

    while (day < 87) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 104.58, 0.6);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.88, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 100.37, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.35, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);

    while (day < 8700) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 8699);
    EXPECT_EQ(state.cycle_range, 40);
    EXPECT_EQ(state.cycle_DOD, 70);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 82.11, 0.5);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.49, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 8700, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingCRate) {
    size_t day = 0;

    // 90 cycle_DOD cycle once per day, slower Crate than above
    std::vector<double> DODs_day = {50., 56.67, 63.33, 70., 76.67, 83.33,
                                    90., 83.33, 76.67, 70., 63.33, 56.67, 50., 43.33, 36.67, 30., 23.33, 16.67,
                                    10., 16.67, 23.33, 30., 36.67, 43.33};

    while (day < 87) {
        for (size_t i = 0; i < DODs_day.size(); i++) {
            size_t idx = day * 24 + i;
            bool charge_changed = i == 7 || i == 19;
            double prev_DOD = DODs_day[i % 24];
            double DOD = DODs_day[i];
            model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
//    EXPECT_EQ(state.nmc_li_neg->DOD_min, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100, 1);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t i = 0; i < DODs_day.size(); i++) {
            size_t idx = day * 24 + i;
            bool charge_changed = i == 7 || i == 19;
            double prev_DOD = DODs_day[i % 24];
            double DOD = DODs_day[i];
            model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 97.61, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.17, 1);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, CyclingCRateMinuteTimestep) {
    double dt_hr = 1. / 60;
    auto steps_per_day = (size_t)(24 / dt_hr);
    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hr));

    size_t day = 0;
    size_t idx = 0;
    // 90 cycle_DOD cycle once per day, slower Crate than above
    std::vector<double> DODs_day = {50., 56.67, 63.33, 70., 76.67, 83.33,
                                    90., 83.33, 76.67, 70., 63.33, 56.67, 50., 43.33, 36.67, 30., 23.33, 16.67,
                                    10., 16.67, 23.33, 30., 36.67, 43.33};

    while (day < 87) {
        for (size_t hr = 0; hr < DODs_day.size(); hr++) {
            double prev_DOD = DODs_day[hr % 24];
            double DOD = DODs_day[hr];
            for (size_t min = 0; min < (size_t)(1. / dt_hr); min++) {
                bool charge_changed = (hr == 7 || hr == 19) && min == 0;
                if (min != 0)
                    prev_DOD = DOD;
                model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
                idx ++;
            }
        }
        day ++;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 86);
//    EXPECT_EQ(state.nmc_li_neg->DOD_min, 43.33);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 103, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100, 1);
    EXPECT_NEAR(state.day_age_of_battery, 87, 1e-3);

    while (day < 870) {
        for (size_t hr = 0; hr < DODs_day.size(); hr++) {
            double prev_DOD = DODs_day[hr % 24];
            double DOD = DODs_day[hr];
            for (size_t min = 0; min < (size_t)(1. / dt_hr); min++) {
                bool charge_changed = (hr == 7 || hr == 19) && min == 0;
                if (min != 0)
                    prev_DOD = DOD;
                model->runLifetimeModels(idx, charge_changed, prev_DOD, DOD, 25);
                idx ++;
            }
        }
        day ++;
    }

    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 869);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 97.61, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.17, 1);
    EXPECT_NEAR(state.day_age_of_battery, 870, 1e-3);
}

/// There's less accuracy since the degradation coefficients from the first day are lost when doing computation on day 2
TEST_F(lib_battery_lifetime_nmc_test, CyclingEveryTwoDays) {
    double T = 25.15;
    size_t day = 0;
    while (day < 87) {
        for (size_t i = 0; i < 48; i++) {
            size_t idx = day * 48 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 90, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i < 47)
                model->runLifetimeModels(idx, false, 10, 10, T);
            else
                model->runLifetimeModels(idx, true, 10, 90, T);
        }
        day += 2;
    }

    auto state = model->get_state();

    EXPECT_EQ(state.n_cycles, 43);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 101.88, 1);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 100.6, 0.5);
    EXPECT_NEAR(state.day_age_of_battery, 88, 1e-3);
}

/** Test focusing on how different time steps affect the integration of a day's degradation in the NMC life model.
 * The integration of degradation is done at the end of each day when the elapsed time, `cum_dt` is exactly 1.
 * Check that if a simulation step has a timestep large enough that `cum_dt` passes from <1 to >1, that the effects
 * on lifetime are the same by breaking that timestep up and accruing the degradation and `cum_dt` correctly
 **/
TEST_F(lib_battery_lifetime_nmc_test, IrregularTimeStep) {
    double T = 35.15;
    auto state = model->get_state();

    auto b_params = std::make_shared<lifetime_params>(model->get_params());
    b_params->dt_hr = 0.5;
    auto b_state = std::make_shared<lifetime_state>(model->get_state());
    auto subhourly_model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(b_params, b_state));

    // run hourly
    size_t day = 0;
    while (day < 87) {
        size_t idx = 0;
        while (idx < 48) {
            size_t i = idx % 24;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 2)
                model->runLifetimeModels(idx, true, 30, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
            idx += 1;
        }
        day += 2;
    }
    state = model->get_state();

    EXPECT_EQ(state.n_cycles, 87);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 104.553, 1e-3);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.92, 1e-3);
    EXPECT_NEAR(state.day_age_of_battery, 88, 1e-3);

    // Now compare with: run 30min timesteps for 23.5 hours then hourly for 24, then 1 0.5 hr time idx
    day = 0;
    while (day < 87) {
        size_t idx = 0;
        while (idx < (size_t) (23.5 * 2)) {
            size_t i = idx;
            if (i == 0)
                subhourly_model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                subhourly_model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 2)
                subhourly_model->runLifetimeModels(idx, true, 30, 50, T);
            else
                subhourly_model->runLifetimeModels(idx, false, 50, 50, T);
            idx += 1;
        }

        b_params->dt_hr = 1;
        b_state = std::make_shared<lifetime_state>(subhourly_model->get_state());
        subhourly_model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(b_params, b_state));
        idx = 0;
        while (idx < 24) {
            size_t i = idx % 24;
            if (i == 0)
                subhourly_model->runLifetimeModels(idx, false, 50, 70, T);
            else if (i == 1)
                subhourly_model->runLifetimeModels(idx, true, 70, 30, T);
            else if (i == 3)
                subhourly_model->runLifetimeModels(idx, true, 30, 50, T);
            else
                subhourly_model->runLifetimeModels(idx, false, 50, 50, T);
            idx += 1;
        }
        b_params->dt_hr = 0.5;
        b_state = std::make_shared<lifetime_state>(subhourly_model->get_state());
        subhourly_model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(b_params, b_state));
        subhourly_model->runLifetimeModels(idx, false, 50, 50, T);
        day += 2;
    }
    state = subhourly_model->get_state();

    EXPECT_EQ(state.n_cycles, 87);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_li, 104.523, 1e-3);
    EXPECT_NEAR(state.nmc_li_neg->q_relative_neg, 103.92, 1e-3);
    EXPECT_NEAR(state.day_age_of_battery, 88, 1e-3);
}

TEST_F(lib_battery_lifetime_nmc_test, TestAgainstKokamData) {
    const char * SSCDIR = std::getenv("SSCDIR");

    std::string kokam_validation_path = std::string(SSCDIR) + "/test/input_cases/battery_nmc_life/";

    std::vector<int> cells_to_test = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};

    dt_hour = 1. / 60 / 60 * 10;

    for (auto cell : cells_to_test) {
        // Get Cell input data
        std::string file_path = kokam_validation_path + "lifetime_validation_cell_" + std::to_string(cell) + ".json";
        std::ifstream file(file_path);

        Json::Value root;
        file >> root;

        std::vector<double> rpt_cycles;
        for (const auto & i : root["rpt_cycles_cum"])
            rpt_cycles.push_back(i.asDouble());

        std::vector<int> days_to_test;
        for (const auto & i : root["rpt_days_cum"])
            days_to_test.push_back((int)round(i.asDouble()));

        std::vector<double> full_soc_profile;
        for (const auto & i : root["15min_profile"])
            full_soc_profile.push_back(i.asDouble());
        dt_hour = 0.25;

        double cell_temp_K = root["temp"].asDouble();

        // Run Life model with the profile, which starts with charging and ends with discharging
        model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));

        // record capacity and cycles elapsed at RPT days, days_to_test
        std::vector<double> life_model_caps;
        std::vector<double> life_model_qLi;
        std::vector<double> life_model_qNeg;
        std::vector<double> cycs;

        // charging = -1; discharging = 1
        int charge_mode = -1;
        int prev_charge_mode = 1;

        for (int i = 1; i < full_soc_profile.size(); i++) {
            int j = i + 1;
            if (j > full_soc_profile.size() - 1)
                j = 0;
            double prev_SOC = full_soc_profile[i];
            double SOC = full_soc_profile[j];

            if (SOC - prev_SOC > 1e-5)
                charge_mode = -1;
            else if (prev_SOC - SOC > 1e-5)
                charge_mode = 1;

            bool charge_changed = false;

            if (charge_mode != prev_charge_mode) {
                charge_changed = true;
            }
            model->runLifetimeModels(i, charge_changed, (1. - prev_SOC) * 100, (1. - SOC) * 100., cell_temp_K - 273.15);
            prev_charge_mode = charge_mode;

            if (model->day_age_of_battery() - days_to_test[0] > -1e-7) {
                auto s = model->get_state();
                life_model_caps.push_back(s.q_relative * 0.75);
                life_model_qLi.push_back(s.nmc_li_neg->q_relative_li * 0.75);
                life_model_qNeg.push_back(s.nmc_li_neg->q_relative_neg * 0.75);
                cycs.push_back(s.n_cycles);
                days_to_test.erase(days_to_test.begin());
            }
            if (days_to_test.empty())
                break;
        }

        // Get Expected Cycle Count & Model Prediction
        std::vector<double> sam_cap_Ah;
        for (const auto & i : root["sam_cap_Ah"])
            sam_cap_Ah.push_back(i.asDouble());

        std::vector<double> sam_cap_cycles;
        for (const auto & i : root["sam_cap_cycles"])
            sam_cap_cycles.push_back(i.asDouble());

        for (size_t n = 0; n < life_model_caps.size(); n++) {
            EXPECT_NEAR(sam_cap_Ah[n], life_model_caps[n], 1e-3) << "cell" << cell;
            EXPECT_NEAR(sam_cap_cycles[n], cycs[n], 1e-3) << "cell" << cell;
        }
    }
}

TEST_F(lib_battery_lifetime_nmc_test, replaceBatteryTest) {
    double tol = 0.01;
    double T = 25.15;
    size_t day = 0;
    while (day < 2000) {
        for (size_t i = 0; i < 24; i++) {
            size_t idx = day * 24 + i;
            if (i == 0)
                model->runLifetimeModels(idx, false, 50, 90, T);
            else if (i == 1)
                model->runLifetimeModels(idx, true, 90, 10, T);
            else if (i == 3)
                model->runLifetimeModels(idx, true, 10, 50, T);
            else
                model->runLifetimeModels(idx, false, 50, 50, T);
        }
        day ++;
    }

    auto s = model->get_state();

    EXPECT_NEAR(s.nmc_li_neg->q_relative_li, 87.90, tol);
    EXPECT_NEAR(s.nmc_li_neg->q_relative_neg, 97.15, tol);

    model->replaceBattery(10);
    s = model->get_state();
    EXPECT_NEAR(s.nmc_li_neg->q_relative_li, 97.90, tol);
    EXPECT_NEAR(s.nmc_li_neg->q_relative_neg, 100, tol);
    EXPECT_NEAR(s.q_relative, 97.90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 0, tol);
    EXPECT_NEAR(s.cycle_range, 80, tol);
    EXPECT_NEAR(s.average_range, 80, tol);
    EXPECT_NEAR(s.n_cycles, 1999, tol);

    model->replaceBattery(100);
    s = model->get_state();
    EXPECT_NEAR(s.nmc_li_neg->q_relative_li, 100, tol);
    EXPECT_NEAR(s.nmc_li_neg->q_relative_li, 100, tol);
    EXPECT_NEAR(s.q_relative, 100, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 0, tol);
    EXPECT_NEAR(s.cycle_range, 0, tol);
    EXPECT_NEAR(s.average_range, 0, tol);
    EXPECT_NEAR(s.n_cycles, 0, tol);
}
