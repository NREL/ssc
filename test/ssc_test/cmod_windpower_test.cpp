#include <gtest/gtest.h>

#include "core.h"
#include "vartab.h"
#include "common.h"
#include "cmod_windpower.h"
#include "cmod_windpower_test.h"

/// Using Wind Resource Data with various Wake Models
TEST_F(CMWindPowerIntegration, ResourceSimpleWake_cmod_windpower){
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e) << "Annual energy.";

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Monthly energy of January";

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e) << "Month energy of December";

}

TEST_F(CMWindPowerIntegration, ResourceWAsp_cmod_windpower){
	ssc_data_set_number(data, "wind_farm_wake_model", 1);
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 32346158, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.7472e6, e);
}

TEST_F(CMWindPowerIntegration, ResourceEddy_cmod_windpower){
	ssc_data_set_number(data, "wind_farm_wake_model", 2);
	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy); 
	EXPECT_NEAR(annual_energy, 31081848, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.6398e6, e);

}

/// Using Weibull Distribution
TEST_F(CMWindPowerIntegration, Weibull_cmod_windpower){
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

/// Using Wind Resource Data
TEST_F(CMWindPowerIntegration, DataSimpleWake_cmod_windpower){
	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(1);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	free_winddata_array(windresourcedata);
}

/// Using 30m Wind Resource File
TEST_F(CMWindPowerIntegration, Resource30mSimpleWake_cmod_windpower){
#ifdef _MSC_VER	
	std::string file = "../../../test/input_docs/wind_30m.srw";
#else	
	std::string file = "../test/input_docs/wind_30m.srw";
#endif
	ssc_data_set_string(data, "wind_resource_filename", file.c_str());

	compute();
	
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	size_t nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 2);
}

/// Using 30m Wind Data
TEST_F(CMWindPowerIntegration, Data30mSimpleWake_cmod_windpower){

	ssc_data_unassign(data, "wind_resource_filename");
	var_data* windresourcedata = create_winddata_array(2);
	var_table *vt = static_cast<var_table*>(data);
	vt->assign("wind_resource_data", *windresourcedata);

	compute();

	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	EXPECT_NEAR(annual_energy, 33224154, e);

	ssc_number_t monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	monthly_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[11];
	EXPECT_NEAR(monthly_energy, 2.8218e6, e);

	size_t nEntries = static_cast<var_table*>(data)->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 2);

	free_winddata_array(windresourcedata);
}