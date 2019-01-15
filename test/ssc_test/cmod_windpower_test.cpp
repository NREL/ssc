#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "cmod_windpower.h"
#include "cmod_windpower_test.h"

/// Measurement heights are different from the turbine's hub height
TEST_F(CMWindPowerIntegration, HubHeightInterpolation_cmod_windpower) {
	// Case 1: hubheight is 200, error
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);
	vt->assign("wind_turbine_hub_ht", 200);

	bool completed = compute(false);
	EXPECT_FALSE(completed) << "Heights difference > 35m";

	// Case 2: hubweight is 90, use shear exponent interpolation
	vt->unassign("wind_turbine_hub_ht");
	vt->assign("wind_turbine_hub_ht", 90);

	compute();
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_GT(annual_energy, 4e06) << "Annual energy should be higher than height at 90";

	free_winddata_array(windresourcedata);
}

/// Using Wind Resource File with various Wake Models
TEST_F(CMWindPowerIntegration, WakeModelsUsingFile_cmod_windpower){
	// Simple Wake Model
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e) << "Annual energy.";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Monthly energy of January";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Month energy of December";

	// WAsp Model
	ssc_data_set_number(data, "wind_farm_wake_model", 1);
	compute();

	annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 32346158, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e);

	// Eddy Viscosity Model
	ssc_data_set_number(data, "wind_farm_wake_model", 2);
	compute();

	annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 31081848, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e);

}

/// Using Interpolated Subhourly Wind Data
TEST_F(CMWindPowerIntegration, UsingInterpolatedSubhourly_cmod_windpower){
	// Using AR Northwestern-Flat Lands
#ifdef _MSC_VER	
	std::string file = "../../../test/input_docs/AR Northwestern-Flat Lands.srw";
#else	
	std::string file = "../test/input_docs/AR Northwestern-Flat Lands.srw";
#endif
	ssc_data_set_string(data, "wind_resource_filename", file.c_str());
	compute();

	ssc_number_t hourly_annual_energy;
	ssc_data_get_number(data, "annual_energy", &hourly_annual_energy);

	ssc_number_t hourly_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];


	// Using 15 min File
#ifdef _MSC_VER	
	file = "../../../test/input_docs/AR Northwestern-Flat Lands-15min.srw";
#else	
	file = "../test/input_docs/AR Northwestern-Flat Lands-15min.srw";
#endif
	ssc_data_set_string(data, "wind_resource_filename", file.c_str());
	compute();

	ssc_number_t check_annual_energy;
	ssc_data_get_number(data, "annual_energy", &check_annual_energy);
	EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005*check_annual_energy);

	ssc_number_t check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005*check_january_energy);

	size_t nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 4);

	// Using 5 min File
#ifdef _MSC_VER	
	file = "../../../test/input_docs/AR Northwestern-Flat Lands-5min.srw";
#else	
	file = "../test/input_docs/AR Northwestern-Flat Lands-5min.srw";
#endif
	ssc_data_set_string(data, "wind_resource_filename", file.c_str());
	compute();

	check_annual_energy;
	ssc_data_get_number(data, "annual_energy", &check_annual_energy);
	EXPECT_NEAR(check_annual_energy, hourly_annual_energy, 0.005*check_annual_energy);

	check_january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(check_january_energy, hourly_january_energy, 0.005*check_january_energy);

	nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 12);
}

/// Using Wind Resource Data
TEST_F(CMWindPowerIntegration, UsingDataArray_cmod_windpower){
	// using hourly data
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();
	double expectedAnnualEnergy = 4219481;
	double relErr = expectedAnnualEnergy * .001;


	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, expectedAnnualEnergy, relErr);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, relErr/10.);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 1972735, relErr/10.);

	free_winddata_array(windresourcedata);

	// 15 min data
	ssc_data_unassign(data, "wind_resource_data");
	windresourcedata = create_winddata_array(4,1);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();

	annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, expectedAnnualEnergy, relErr);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, relErr / 10.);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 1972735, relErr / 10.);

	int gen_length = 0;
	ssc_data_get_array(data, "gen", &gen_length);
	EXPECT_EQ(gen_length, 8760 * 4);

	free_winddata_array(windresourcedata);
}

/// Using Weibull Distribution
TEST_F(CMWindPowerIntegration, Weibull_cmod_windpower) {
	ssc_data_set_number(data, "wind_resource_model_choice", 1);
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 180453760, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 15326247, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 15326247, e);
}


/// Icing and Low Temp Cutoff, with Wind Resource Data
TEST_F(CMWindPowerIntegration, IcingAndLowTempCutoff_cmod_windpower) {
	//modify test inputs
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1,1);
	float rh[8760];
	for (unsigned int i = 0; i < 8760; i++) {
		if (i % 2 == 0) rh[i] = 0.75f;
		else rh[i] = 0.0f;
	}
	var_data rh_vd = var_data(rh, 8760);
	windresourcedata->table.assign("rh", rh_vd);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);
	vt->assign("en_low_temp_cutoff", 1);
	vt->assign("en_icing_cutoff", 1);
	vt->assign("low_temp_cutoff", 40.f);
	vt->assign("icing_cutoff_temp", 55.f);
	vt->assign("icing_cutoff_rh", 0.70f);

	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 2108935, e) << "Reduced annual energy";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 0, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 986114, e);

	ssc_number_t losses_percent;
	ssc_data_get_number(data, "cutoff_losses", &losses_percent);
	EXPECT_NEAR(losses_percent, 0.5, 0.01);

	free_winddata_array(windresourcedata);
}

