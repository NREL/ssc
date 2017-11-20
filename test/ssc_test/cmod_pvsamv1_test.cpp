#include <gtest/gtest.h>

#include "cmod_pvsamv1_test.h"
#include "../input_cases/pvsamv1_cases.h"

/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMPvsamv1PowerIntegration, DefaultNoFinancialModel){
	
	ssc_data_t data = ssc_data_create();
	bool pvsam_errors = pvsam_nofinancial_pheonix(data);
	EXPECT_FALSE(pvsam_errors);

	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		ssc_number_t performance_ratio;
		ssc_data_get_number(data, "performance_ratio", &performance_ratio);

		EXPECT_NEAR(annual_energy, 8714, m_error_tolerance_hi) << "Annual energy.";
		EXPECT_NEAR(capacity_factor, 21.2, m_error_tolerance_lo) << "Capacity factor";
		EXPECT_NEAR(kwh_per_kw, 1857, m_error_tolerance_hi) << "Energy yield";
		EXPECT_NEAR(performance_ratio, 0.79, m_error_tolerance_lo) << "Energy yield";
		ssc_data_free(data);
	}
}
