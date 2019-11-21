#include <numeric>

#include <gtest/gtest.h>

#include "vartab.h"

#include "cmod_battery_test.h"


/// Test standalone battery compute modeule with a input lifetime generation and commercial load
TEST_F(CMBattery_cmod_battery, CommercialLifetimePeakShaving) {

	// Run with fixed output
	ssc_number_t n_years;
	ssc_data_get_number(data, "analysis_period", &n_years);
	size_t n_lifetime = (size_t)(n_years) * 8760;

	int errors = run_module(data, "battery");
	EXPECT_FALSE(errors);

	if (!errors)
	{
		// roundtrip efficiency test will ensure that the battery cycled
		ssc_number_t roundtripEfficiency;
		ssc_data_get_number(data, "average_battery_roundtrip_efficiency", &roundtripEfficiency);
		EXPECT_NEAR(roundtripEfficiency, 94.129, 2);

		// test that lifetime output is achieved
		int n;
		calculated_array = ssc_data_get_array(data, "gen", &n);
		EXPECT_EQ(n_lifetime, (size_t)n);

		// test that battery was replaced at some point
		calculated_array = ssc_data_get_array(data, "batt_bank_replacement", &n);
		int replacements = std::accumulate(calculated_array, calculated_array + n, 0);
		
		EXPECT_GT(replacements, 0);
	}
}

TEST_F(CMBattery_cmod_battery, ResilienceMetricsFullLoad){
    auto data_vtab = static_cast<var_table*>(data);
    data_vtab->assign("crit_load", data_vtab->as_vector_ssc_number_t("load"));
    data_vtab->assign("system_use_lifetime_output", 0);
    data_vtab->assign("analysis_years", 1);
    data_vtab->assign("gen", var_data(data_vtab->as_array("gen", nullptr), 8760));
    data_vtab->assign("batt_replacement_option", 0);

    int errors = run_module(data, "battery");
    EXPECT_FALSE(errors);

    auto resilience_hours = data_vtab->as_vector_ssc_number_t("resilience_hrs");
    double resilience_hrs_min = data_vtab->as_number("resilience_hrs_min");
    double resilience_hrs_max = data_vtab->as_number("resilience_hrs_max");
    double resilience_hrs_avg = data_vtab->as_number("resilience_hrs_avg");
    auto outage_durations = data_vtab->as_vector_ssc_number_t("outage_durations");
    auto probs_of_surviving = data_vtab->as_vector_ssc_number_t("probs_of_surviving");
    double avg_critical_load = data_vtab->as_double("avg_critical_load");

    EXPECT_EQ(resilience_hours[0], 0);
    EXPECT_EQ(resilience_hours[1], 1);
    EXPECT_NEAR(avg_critical_load, 878.2, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 1.22, 0.01);
    EXPECT_EQ(resilience_hrs_min, 0);
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(resilience_hrs_max, 23);
    EXPECT_EQ(outage_durations[23], 23);
    EXPECT_NEAR(probs_of_surviving[0], 0.687, 1e-3);
    EXPECT_NEAR(probs_of_surviving[1], 0.070, 1e-3);
}