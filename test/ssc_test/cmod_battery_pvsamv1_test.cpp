#include <gtest/gtest.h>

#include "cmod_battery_pvsamv1_test.h"

#include "../input_cases/weather_inputs.h"
#include "../input_cases/pvsam1_battery_common_data.h"

void daily_battery_stats::compute(std::vector<ssc_number_t> batt_power_data) {
	// Initialize default values
	peakCycles = 0;
	avgCycles = 0;

	size_t index = 0;
	size_t n = batt_power_data.size();
	int cycleState = 0; // -1 for charging, 1 for discharging;
	ssc_number_t charges = 0;
	ssc_number_t discharges = 0;
	bool halfCycle = false;
	while (index < n) {
		int cycles = 0;
		for (int hour = 0; hour < 24; hour++) {
			ssc_number_t currentPower = batt_power_data[index];

			if (currentPower < 0) {
				if (cycleState != -1) {
					if (halfCycle) {
						cycles++;
						halfCycle = false;
					}
					else {
						halfCycle = true;
					}
				}
				cycleState = -1;
			}

			if (currentPower > 0) {
				if (cycleState != 1) {
					if (halfCycle) {
						cycles++;
						halfCycle = false;
					}
					else {
						halfCycle = true;
					}
				}
				cycleState = 1;
			}

			index++;
		}
		if (cycles > peakCycles) {
			peakCycles = cycles;
		}
		avgCycles += cycles;
		
	}
	ssc_number_t days = n / 24.0;
	avgCycles = avgCycles / days;
	peakKwDischarge = *std::max_element(batt_power_data.begin(), batt_power_data.end());
	peakKwCharge = *std::min_element(batt_power_data.begin(), batt_power_data.end());
}

TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, TestDailyBatteryStats)
{
	// 48 hrs of battery data to test the compute function
	std::vector<ssc_number_t> batt_power_data = { 0, 1, 0, -1, 0, 2, 0, -2, 0, 3, -3, 4, -1, 6, -4, -1, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 1, 0, 0, 0, -1, -1, -1};
	EXPECT_EQ(batt_power_data.size(), 48);
	daily_battery_stats batt_stats = daily_battery_stats(batt_power_data);

	EXPECT_EQ(batt_stats.peakKwCharge, -4);
	EXPECT_EQ(batt_stats.peakKwDischarge, 6);
	EXPECT_EQ(batt_stats.peakCycles, 5);
	EXPECT_NEAR(batt_stats.avgCycles, 3, 0.1);
}

/// Test PVSAMv1 with all defaults and battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACBatteryModelIntegration)
{
	ssc_data_t data = ssc_data_create();
	pvsamv_nofinancial_default(data);
	battery_data_default(data);

	std::map<std::string, double> pairs;
	pairs["en_batt"] = 1;
	pairs["batt_ac_or_dc"] = 1; //AC
	pairs["analysis_period"] = 1;
	set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers

	ssc_number_t expectedEnergy[3] = { 8594, 8594, 8689 };
	ssc_number_t expectedBatteryChargeEnergy[3] = { 1442, 1443, 258 };
	ssc_number_t expectedBatteryDischargeEnergy[3] = { 1321, 1323, 233 };

	ssc_number_t peakKwCharge[3] = { -2.81, -2.87, -2.25 };
	ssc_number_t peakKwDischarge[3] = { 1.39, 1.30, 0.97 };
	ssc_number_t peakCycles[3] = { 2, 2, 1 };
	ssc_number_t avgCycles[3] = { 1.1945, 1.1014, 0.4904 };

	// Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
	for (int i = 0; i < 3; i++) {
		pairs["batt_dispatch_choice"] = i;

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);

		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

			auto data_vtab = static_cast<var_table*>(data);
			auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
			EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";
			
			auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
			EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

			auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
			daily_battery_stats batt_stats = daily_battery_stats(batt_power);

			EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
		}
	}
}

/// Test PVSAMv1 with all defaults and battery enabled with custom dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACDCBatteryModelIntegrationCustomDispatch)
{
	ssc_data_t data = ssc_data_create();
	pvsamv_nofinancial_default(data);
	battery_data_default(data);

	std::map<std::string, double> pairs;
	pairs["en_batt"] = 1;

	pairs["analysis_period"] = 1;
	set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers
	pairs["batt_dispatch_choice"] = 3;
	set_array(data, "batt_custom_dispatch", custom_dispatch_residential_schedule, 8760);

	ssc_number_t expectedEnergy[2] = { 8710, 8717 };
	ssc_number_t expectedBatteryChargeEnergy[2] = { 4.6, 4.7};
	ssc_number_t expectedBatteryDischargeEnergy[2] = { 0.76, 7.6};

	ssc_number_t peakKwCharge[2] = { -2.7, -2.5};
	ssc_number_t peakKwDischarge[2] = { 0.6, 4.0};
	ssc_number_t peakCycles[2] = { 1, 1 };
	ssc_number_t avgCycles[2] = { 0.0027, 0.0027};

	// Test both AC and DC using the same dispatch model
	for (int i = 0; i < 2; i++) {
		pairs["batt_ac_or_dc"] = i;

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);

		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

			auto data_vtab = static_cast<var_table*>(data);
			auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
			EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

			auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
			EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

			auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
			daily_battery_stats batt_stats = daily_battery_stats(batt_power);

			EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001); // Runs once per year
		}
	}
}

/// Test PVSAMv1 with all defaults and battery enabled with manual dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACDCBatteryModelIntegrationManualDispatch)
{
	ssc_data_t data = ssc_data_create();
	pvsamv_nofinancial_default(data);
	battery_data_default(data);

	std::map<std::string, double> pairs;
	pairs["en_batt"] = 1;

	pairs["analysis_period"] = 1;
	set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers
	pairs["batt_dispatch_choice"] = 4;

	ssc_number_t expectedEnergy[2] = { 8701, 8684 };
	ssc_number_t expectedBatteryChargeEnergy[2] = { 468, 477 };
	ssc_number_t expectedBatteryDischargeEnergy[2] = { 437, 446 };

	ssc_number_t peakKwCharge[2] = { -2.37, -2.27 };
	ssc_number_t peakKwDischarge[2] = { 1.31, 1.31 };
	ssc_number_t peakCycles[2] = { 2, 2 };
	ssc_number_t avgCycles[2] = { 0.7178, 0.7205 };

	// Test both AC and DC using the same dispatch model
	for (int i = 0; i < 2; i++) {
		pairs["batt_ac_or_dc"] = i;

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);

		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

			auto data_vtab = static_cast<var_table*>(data);
			auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
			EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

			auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
			EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

			auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
			daily_battery_stats batt_stats = daily_battery_stats(batt_power);

			EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
		}
	}
}

/// Test PVSAMv1 with all defaults and DC battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialDCBatteryModelIntegration)
{
	ssc_data_t data = ssc_data_create();
	pvsamv_nofinancial_default(data);
	battery_data_default(data);

	std::map<std::string, double> pairs;
	pairs["en_batt"] = 1;
	pairs["batt_ac_or_dc"] = 0; //DC
	pairs["analysis_period"] = 1;
	set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers

	ssc_number_t expectedEnergy[3] = { 8634, 8637, 8703 };
	ssc_number_t expectedBatteryChargeEnergy[3] = { 1373, 1377, 245 };
	ssc_number_t expectedBatteryDischargeEnergy[3] = { 1245, 1249, 219 };

	ssc_number_t peakKwCharge[3] = { -3.07, -2.87, -2.26 };
	ssc_number_t peakKwDischarge[3] = { 1.4, 1.29, 0.95 };
	ssc_number_t peakCycles[3] = { 2, 1, 1 };
	ssc_number_t avgCycles[3] = { 1.0027, 0.9973, 0.4795 };

	// Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
	for (int i = 0; i < 3; i++) {
		pairs["batt_dispatch_choice"] = i;

		int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
		EXPECT_FALSE(pvsam_errors);

		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

			auto data_vtab = static_cast<var_table*>(data);
			auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
			EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

			auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
			EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

			auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
			daily_battery_stats batt_stats = daily_battery_stats(batt_power);

			EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
		}
	}
}

/// Test PVSAMv1 with all defaults and battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_ACBatteryModelIntegration)
{
	ssc_data_t data = ssc_data_create();
	pvsamv1_pv_defaults(data);
	pvsamv1_battery_defaults(data);
	grid_and_rate_defaults(data);
	singleowner_defaults(data);

	ssc_number_t expectedEnergy[3] = { 37319944, 37276643, 37276643 };
	ssc_number_t expectedBatteryChargeEnergy[3] = { 15751, 315437, 315437 }; // No rate model means battery use is low
	ssc_number_t expectedBatteryDischargeEnergy[3] = { 14847, 271231, 271231 };

	ssc_number_t peakKwCharge[3] = { -1030.6, -1051.5, -1051.5 };
	ssc_number_t peakKwDischarge[3] = { 967.5, 969.5, 969.5 };
	ssc_number_t peakCycles[3] = { 1, 1, 1};
	ssc_number_t avgCycles[3] = { 0.0158, 0.2356, 0.2356 };

	// Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
	for (int i = 0; i < 3; i++) {
		ssc_data_set_number(data, "batt_dispatch_choice", i);

		int pvsam_errors = run_pvsam1_battery_ppa(data);
		EXPECT_FALSE(pvsam_errors);

		if (!pvsam_errors)
		{
			ssc_number_t annual_energy;
			ssc_data_get_number(data, "annual_energy", &annual_energy);
			EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

			auto data_vtab = static_cast<var_table*>(data);
			auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
			EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

			auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
			EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

			auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
			daily_battery_stats batt_stats = daily_battery_stats(batt_power);

			EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
			EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
		}
	}
}

/// Test PVSAMv1 with all defaults and battery enabled with manual dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_ManualDispatchBatteryModelIntegration)
{
	ssc_data_t data = ssc_data_create();
	pvsamv1_pv_defaults(data);
	pvsamv1_battery_defaults(data);
	grid_and_rate_defaults(data);
	singleowner_defaults(data);

	ssc_number_t expectedEnergy = 37145455;
	ssc_number_t expectedBatteryChargeEnergy = 1298143;
	ssc_number_t expectedBatteryDischargeEnergy = 1122750;

	ssc_number_t peakKwCharge = -1052.1;
	ssc_number_t peakKwDischarge = 874.7;
	ssc_number_t peakCycles = 1;
	ssc_number_t avgCycles = 1;

	ssc_data_set_number(data, "batt_dispatch_choice", 4);

	// Modify utility rate to Salt River Project Super Peak
	ssc_number_t p_ur_ec_sched_weekday_srp[288] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6 };
	ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday_srp, 12, 24);
	ssc_number_t p_ur_ec_sched_weekend_srp[288] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 };
	ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend_srp, 12, 24);
	ssc_number_t p_ur_ec_tou_mat_srp[36] = { 1, 1, 9.9999999999999998e+37, 0, 0.2969, 0, 2, 1, 9.9999999999999998e+37, 0, 0.081900000000000001, 0, 3, 1, 9.9999999999999998e+37, 0, 0.34989999999999999, 0, 4, 1, 9.9999999999999998e+37, 0, 0.083599999999999994, 0, 5, 1, 9.9999999999999998e+37, 0, 0.123, 0, 6, 1, 9.9999999999999998e+37, 0, 0.074999999999999997, 0 };
	ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat_srp, 6, 6);

	int pvsam_errors = run_pvsam1_battery_ppa(data);
	EXPECT_FALSE(pvsam_errors);

	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

		auto data_vtab = static_cast<var_table*>(data);
		auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
		EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

		auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
		EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

		auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
		daily_battery_stats batt_stats = daily_battery_stats(batt_power);

		EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
	}
}

/// Test PVSAMv1 with all defaults and DC battery enabled with custom dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_CustomDispatchBatteryModelDCIntegration)
{
	ssc_data_t data = ssc_data_create();
	pvsamv1_pv_defaults(data);
	pvsamv1_battery_defaults(data);
	grid_and_rate_defaults(data);
	singleowner_defaults(data);

	ssc_number_t expectedEnergy = 37321520;
	ssc_number_t expectedBatteryChargeEnergy = 2030;
	ssc_number_t expectedBatteryDischargeEnergy = 3175;

	ssc_number_t peakKwCharge = -1020.4;
	ssc_number_t peakKwDischarge = 958.7;
	ssc_number_t peakCycles = 1;
	ssc_number_t avgCycles = 0.0027;

	ssc_data_set_number(data, "batt_dispatch_choice", 3);
	ssc_data_set_number(data, "batt_ac_or_dc", 0);
	set_array(data, "batt_custom_dispatch", custom_dispatch_singleowner_schedule, 8760);

	int pvsam_errors = run_pvsam1_battery_ppa(data);
	EXPECT_FALSE(pvsam_errors);

	if (!pvsam_errors)
	{
		ssc_number_t annual_energy;
		ssc_data_get_number(data, "annual_energy", &annual_energy);
		EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

		auto data_vtab = static_cast<var_table*>(data);
		auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
		EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

		auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
		EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

		auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
		daily_battery_stats batt_stats = daily_battery_stats(batt_power);

		EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
		EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001); // Runs once per year
	}

}
