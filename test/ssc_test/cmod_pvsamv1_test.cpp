#include <gtest/gtest.h>

#include "cmod_pvsamv1_test.h"
#include "../input_cases/pvsamv1_cases.h"

/// Test PVSAMv1 with all defaults and no-financial model
TEST_F(CMPvsamv1PowerIntegration, DefaultNoFinancialModel){
	
	int pvsam_errors = run_module(data, "pvsamv1");

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
	}
}

/// Test PVSAMv1 with all defaults and residential financial model
TEST_F(CMPvsamv1PowerIntegration, DefaultResidentialModel)
{

	ssc_data_t data = ssc_data_create();
	int pvsam_errors = pvsam_residential_pheonix(data);
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
	}
}

/// Test PVSAMv1 with default no-financial model and a 15-minute weather file 
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelCustomWeatherFile) {

	std::map<std::string, std::string> pairs; 
	pairs["solar_resource_file"] = solar_resource_path_15_min;
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);

	EXPECT_FALSE(pvsam_errors);

	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 7587, m_error_tolerance_hi) << "Annual energy.";

		ssc_number_t capacity_factor;
		ssc_data_get_number(data, "capacity_factor", &capacity_factor);
		EXPECT_NEAR(capacity_factor, 18.5, m_error_tolerance_lo) << "Capacity factor";

		ssc_number_t kwh_per_kw;
		ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
		EXPECT_NEAR(kwh_per_kw, 1617, m_error_tolerance_hi) << "Energy yield";

		ssc_number_t performance_ratio;
		ssc_data_get_number(data, "performance_ratio", &performance_ratio);
		EXPECT_NEAR(performance_ratio, 0.80, m_error_tolerance_lo) << "Energy yield";
	}
}

/// Test PVSAMv1 with default no-financial model and combinations of Sky Diffuse Model and Weather File Irradiance
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelSkyDiffuseAndIrradModels) 
{
	std::vector<double> annual_energy_expected = { 8513, 8522, 8525, 8635, 8645, 8647, 8714, 8723, 8726, 7623, 7377};
	std::map<std::string, double> pairs;
	size_t count = 0;

	// Sky diffuse models: isotropic, hdkr, perez
	for (int sky_diffuse_model = 0; sky_diffuse_model < 3; sky_diffuse_model++)
	{
		// Weather file irradiance: DNI & DHI, DNI & GHI, GHI & DHI
		for (int irrad_mode = 0; irrad_mode < 3; irrad_mode++)
		{
			pairs["irrad_mode"] = irrad_mode;
			pairs["sky_model"] = sky_diffuse_model;			
			int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
			EXPECT_FALSE(pvsam_errors);
			if (!pvsam_errors)
			{
				ssc_number_t annual_energy;
				ssc_data_get_number(data, "annual_energy", &annual_energy);
				EXPECT_NEAR(annual_energy, annual_energy_expected[count], m_error_tolerance_hi) << "Annual energy.";
			}
			count++;
		}
	}

	// Perez with POA reference cell
	pairs["sky_model"] = 2;
	pairs["irrad_mode"] = 3;
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[count], m_error_tolerance_hi);
		count++;
	}

	// Perez with POA pyranometer
	pairs["irrad_mode"] = 4;
	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[count], m_error_tolerance_hi);
	}
}
	
/// Test PVSAMv1 with default no-financial model and combinations of module and inverter models
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelModuleAndInverterModels)
{
	std::vector<double> annual_energy_expected = { 2518, 2548, 2476, 2518, 8714, 8694, 8661, 8714, 54, 57, 60, 54, 5405, 5400, 5347, 5404, 1767, 1807, 1736, 1767};
	std::map<std::string, double> pairs;
	size_t count = 0;

	// Module models: Simple Efficiency, CEC Performance Database, CEC User Entered, Sandia, IEC61853
	for (int module_model = 0; module_model < 5; module_model++)
	{
		// Inverter models: CEC, Datasheet, Partload Curve, Coefficient Generator
		for (int inverter_model = 0; inverter_model < 4; inverter_model++)
		{
			pairs["module_model"] = module_model;
			pairs["inverter_model"] = inverter_model;
			int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
			EXPECT_FALSE(pvsam_errors);

			if (!pvsam_errors)
			{
				ssc_number_t annual_energy;
				ssc_data_get_number(data, "annual_energy", &annual_energy);
				EXPECT_NEAR(annual_energy, annual_energy_expected[count], m_error_tolerance_hi) << "Annual energy.";
			}
			count++;
		}
	}
}

/// Test PVSAMv1 with default no-financial model and sytem design page changes
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelSystemDesign)
{
	pvsamv_nofinancial_default(data);

	// Specify modules and inverters with tracking options
	// Tracking options: Fixed, 1-axis, 2-axis, Azimuth Axis, Seasonal Tilt
	std::map<std::string, double> pairs;
	pairs["modules_per_string"] = 6;
	pairs["strings_in_parallel"] = 49;
	pairs["inverter_count"] = 22;
	pairs["subarray1_track_mode"] = 0;

	std::vector<double> annual_energy_expected = { 183243, 242540, 258572, 216242, 192975 };

	for (int tracking_option = 0; tracking_option != 5; tracking_option++)
	{
		// update tracking option
		pairs["subarray1_track_mode"] = (double)tracking_option;
		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);
		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[tracking_option], m_error_tolerance_hi) << "Annual energy.";
		}
	}

	// Test fixed-tilt with backtracking
	pairs["subarray1_track_mode"] = 1;
	pairs["subarray1_backtrack"] = 1;

	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, 237340, m_error_tolerance_hi) << "Annual energy.";
	}

	// Test multiple sub-arrays with different tracking, tilt, azimuth, gcr, tracker rotation limit
	
	pairs["subarray2_enable"] = 1;
	pairs["subarray2_nstrings"] = 15;
	pairs["subarray3_enable"] = 1;
	pairs["subarray3_nstrings"] = 10;
	pairs["subarray4_enable"] = 1;
	pairs["subarray4_nstrings"] = 10;

	annual_energy_expected.clear();
	std::vector<double> subarray1_azimuth = {0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180};
	std::vector<double> subarray2_azimuth = { 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray3_azimuth = { 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> subarray4_azimuth = { 180, 180, 180, 180, 180, 180, 180, 180, 180, 0, 90, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180 };
	std::vector<double> enable_mismatch = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray1_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray2_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray3_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray4_gcr = { 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.5, 0.9, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 };
	std::vector<double> subarray1_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray2_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray3_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray4_tilt = { 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 0, 45, 90, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20 };
	std::vector<double> subarray1_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray2_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray3_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray4_rotlim = { 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45 };
	std::vector<double> subarray1_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray2_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	std::vector<double> subarray3_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0 };
	std::vector<double> subarray4_track_mode = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4 };
	annual_energy_expected = { 167392, 176331, 183243, 166251, 175833, 183243, 171952, 178321, 183243, 171952, 178321, 183243, 183243, 183227, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 183243, 177310, 182927, 162456, 176883, 182902, 160961, 179014, 183024, 168431, 179014, 183024, 168431, 183243, 183243, 183243, 183243, 183243, 198796, 205187, 192695, 186088, 183243, 201501, 206750, 193370, 186290, 183243, 195419, 198925, 189995, 185277, 183243, 195419, 198925, 189995, 185277 };

	for (int i = 0; i != annual_energy_expected.size(); i++)
	{
		pairs["enable_mismatch_vmax_calc"] = enable_mismatch[i];
		pairs["subarray1_azimuth"] = subarray1_azimuth[i];
		pairs["subarray2_azimuth"] = subarray2_azimuth[i]; 
		pairs["subarray3_azimuth"] = subarray3_azimuth[i]; 
		pairs["subarray4_azimuth"] = subarray4_azimuth[i];
		pairs["subarray1_gcr"] = subarray1_gcr[i]; 
		pairs["subarray2_gcr"] = subarray2_gcr[i]; 
		pairs["subarray3_gcr"] = subarray3_gcr[i]; 
		pairs["subarray4_gcr"] = subarray4_gcr[i];
		pairs["subarray1_tilt"] = subarray1_tilt[i]; 
		pairs["subarray2_tilt"] = subarray2_tilt[i]; 
		pairs["subarray3_tilt"] = subarray3_tilt[i]; 
		pairs["subarray4_tilt"] = subarray4_tilt[i];
		pairs["subarray1_rotlim"] = subarray1_rotlim[i]; 
		pairs["subarray2_rotlim"] = subarray2_rotlim[i]; 
		pairs["subarray3_rotlim"] = subarray3_rotlim[i]; 
		pairs["subarray4_rotlim"] = subarray4_rotlim[i];
		pairs["subarray1_track_mode"] = subarray1_track_mode[i]; 
		pairs["subarray2_track_mode"] = subarray2_track_mode[i]; 
		pairs["subarray3_track_mode"] = subarray3_track_mode[i]; 
		pairs["subarray4_track_mode"] = subarray4_track_mode[i];

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);
		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, annual_energy_expected[i], m_error_tolerance_hi) << "Index: " << i;
		}
	}
}

/// Test PVSAMv1 with default no-financial model and different shading options
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelShading)
{
	// 0: No Shading, 1: 3D Shading, 2: 3D shading with self shading (non-linear), 3: Snow
	std::vector<double> annual_energy_expected = { 12911, 10607, 10579, 10377};
	std::map<std::string, double> pairs;

	// 2 subarrays, one pointing east, one west
	pairs["modules_per_string"] = 6;
	pairs["strings_in_parallel"] = 4;
	pairs["inverter_count"] = 2;
	pairs["subarray1_azimuth"] = 90;
	pairs["subarray2_enable"] = 1;
	pairs["subarray2_nstrings"] = 2;
	pairs["subarray2_azimuth"] = 270;

	// 0. No Shading
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[0], m_error_tolerance_hi);
	}

	// 1. Add 3D Shading
	set_matrix(data, "subarray1_shading:timestep", subarray1_shading, 8760, 2);
	set_matrix(data, "subarray2_shading:timestep", subarray2_shading, 8760, 2);
	pairs["subarray1_shading:diff"] =  10.010875701904297;
	pairs["subarray2_shading:diff"] = 10.278481483459473;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[1], m_error_tolerance_hi);
	}
	
	// 2. Add Self Shading to 3D shading
	pairs["subarray1_shade_mode"] = 1;
	pairs["subarray1_mod_orient"] = 1;
	pairs["subarray1_nmody"] = 1;
	pairs["subarray1_nmodx"] = 6;
	pairs["subarray2_shade_mode"] = 1;
	pairs["subarray2_mod_orient"] = 1;
	pairs["subarray2_nmody"] = 1;
	pairs["subarray2_nmodx"] = 6;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[2], m_error_tolerance_hi);
	}

	// 3. Add Snow losses to all shading
	pairs["en_snow_model"] = 1;
	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[3], m_error_tolerance_hi);
	}

}

/// Test PVSAMv1 with default no-financial model and different loss options
TEST_F(CMPvsamv1PowerIntegration, NoFinancialModelLosses)
{
	// 0: Default Losses, 1: Modify Point Losses, 2: Modify Availability
	std::vector<double> annual_energy_expected = { 8714, 7874, 7607 };
	std::map<std::string, double> pairs;

	// 0: Default losses
	int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[0], m_error_tolerance_hi);
	}

	// 1: Modify Point Losses
	ssc_number_t p_subarray1_soiling[12] = { 5, 5, 5, 5, 6, 6, 6, 6, 5, 5, 5, 5 };
	ssc_data_set_array(data, "subarray1_soiling", p_subarray1_soiling, 12);

	pairs["subarray1_mismatch_loss"] = 3;
	pairs["subarray1_diodeconn_loss"] = 0.6;
	pairs["subarray1_dcwiring_loss"] = 2;
	pairs["subarray1_tracking_loss"] = 1;
	pairs["subarray1_nameplate_loss"] = 1;
	pairs["dcoptimizer_loss"] = 1;
	pairs["acwiring_loss"] = 2;
	pairs["transformer_no_load_loss"] = 1;
	pairs["transformer_load_loss"] = 1;

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[1], m_error_tolerance_hi);
	}

	// 2. Modify availability losses
	ssc_number_t p_adjust[3] = { 5268, 5436, 50 };
	ssc_data_set_matrix(data, "adjust:periods", p_adjust, 1, 3);
	ssc_number_t p_dc_adjust[3] = { 5088, 5256, 100 };
	ssc_data_set_matrix(data, "dc_adjust:periods", p_dc_adjust, 1, 3);

	pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
	EXPECT_FALSE(pvsam_errors);
	if (!pvsam_errors) {
		SetCalculated("annual_energy");
		EXPECT_NEAR(calculated_value, annual_energy_expected[2], m_error_tolerance_hi);
	}
}