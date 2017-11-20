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
		EXPECT_NEAR(annual_energy, 8714, m_error_tolerance_hi) << "Annual energy.";

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 21.2, m_error_tolerance_lo) << "Capacity factor";

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 1857, m_error_tolerance_hi) << "Energy yield";

		ssc_number_t performance_ratio;
		ssc_data_get_number(data, "performance_ratio", &performance_ratio);
		EXPECT_NEAR(performance_ratio, 0.79, m_error_tolerance_lo) << "Energy yield";

		ssc_data_free(data);
	}
}

/// Test PVSAMv1 with all defaults and residential financial model
TEST_F(CMPvsamv1PowerIntegration, DefaultResidentialModel)
{

	ssc_data_t data = ssc_data_create();
	bool pvsam_errors = pvsam_residential_pheonix(data);
	EXPECT_FALSE(pvsam_errors);

	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 8714, m_error_tolerance_hi) << "Annual energy.";

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 21.2, m_error_tolerance_lo) << "Capacity factor";

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 1857, m_error_tolerance_hi) << "Energy yield";

		ssc_number_t performance_ratio;
		ssc_data_get_number(data, "performance_ratio", &performance_ratio);
		EXPECT_NEAR(performance_ratio, 0.79, m_error_tolerance_lo) << "Energy yield";

		ssc_number_t lcoe_nom;
		ssc_data_get_number(data, "lcoe_nom", &lcoe_nom);
		EXPECT_NEAR(lcoe_nom, 7.14, m_error_tolerance_lo) << "Levelized COE (nominal)";

		ssc_number_t lcoe_real;
		ssc_data_get_number(data, "lcoe_real", &lcoe_real);
		EXPECT_NEAR(lcoe_real, 5.65, m_error_tolerance_lo) << "Levelized COE (real)";

		ssc_number_t elec_cost_without_system_year1;
		ssc_data_get_number(data, "elec_cost_without_system_year1", &elec_cost_without_system_year1);
		EXPECT_NEAR(elec_cost_without_system_year1, 973, m_error_tolerance_hi) << "Electricity bill without system (year 1)";

		ssc_number_t elec_cost_with_system_year1;
		ssc_data_get_number(data, "elec_cost_with_system_year1", &elec_cost_with_system_year1);
		EXPECT_NEAR(elec_cost_with_system_year1, 125, m_error_tolerance_hi) << "Electricity bill with system (year 1)";

		ssc_number_t savings_year1;
		ssc_data_get_number(data, "savings_year1", &savings_year1);
		EXPECT_NEAR(savings_year1, 848, m_error_tolerance_hi) << "Net savings with system (year 1)";

		ssc_number_t npv;
		ssc_data_get_number(data, "npv", &npv);
		EXPECT_NEAR(npv, 4648, m_error_tolerance_hi) << "Net present value";

		ssc_number_t payback;
		ssc_data_get_number(data, "payback", &payback);
		EXPECT_NEAR(payback, 11.8, m_error_tolerance_lo) << "Payback period";

		ssc_number_t discounted_payback;
		ssc_data_get_number(data, "discounted_payback", &discounted_payback);
		EXPECT_NEAR(discounted_payback, 22.9, m_error_tolerance_lo) << "Discounted payback period";

		ssc_number_t adjusted_installed_cost;
		ssc_data_get_number(data, "adjusted_installed_cost", &adjusted_installed_cost);
		EXPECT_NEAR(adjusted_installed_cost, 13758, m_error_tolerance_hi) << "Net capital cost";

		ssc_number_t first_cost;
		ssc_data_get_number(data, "first_cost", &first_cost);
		EXPECT_NEAR(first_cost, 0, m_error_tolerance_lo) << "Equity";

		ssc_number_t loan_amount;
		ssc_data_get_number(data, "loan_amount", &loan_amount);
		EXPECT_NEAR(loan_amount, 13758, m_error_tolerance_hi) << "Debt";

		ssc_data_free(data);
	}
}