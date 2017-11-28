#include <gtest/gtest.h>

#include "core.h"
#include "vartab.h"
#include "common.h"
#include "cmod_windpower.h"
#include "cmod_windpower_test.h"

// weatherfile interpolation tests?

/// Testing various Wake Models
TEST_F(CMWindPowerIntegration, DISABLED_ResourceSimpleWake_cmod_windpower){
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 33224154, e) << "Annual energy.";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.8218e6, e) << "Monthly energy of January";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.8218e6, e) << "Month energy of December";
}

TEST_F(CMWindPowerIntegration, DISABLED_ResourceWAsp_cmod_windpower){
	modify_var(vartab, "wind_farm_wake_model", 1);

	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 32346158, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.7472e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.7472e6, e);
}

TEST_F(CMWindPowerIntegration, ResourceEddy_cmod_windpower){
	modify_var(vartab, "wind_farm_wake_model", 2);
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);

	EXPECT_NEAR(ann_energy, 31081848, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.6398e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.6398e6, e);

}

/// Using Weibull Distribution
TEST_F(CMWindPowerIntegration, DISABLED_Weibull_cmod_windpower){
	modify_var(vartab, "wind_resource_model_choice", 1);
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);

	EXPECT_NEAR(ann_energy, 180453760, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 15326247, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 15326247, e);
}

/// Using wind_resource_data instead
TEST_F(CMWindPowerIntegration, DISABLED_DataSimpleWake_cmod_windpower){
	vartab->unassign("wind_resource_filename");
	vartab->assign("wind_resource_data", *windResourceData);
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 33224154, e) << "Annual energy.";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.8218e6, e) << "Monthly energy of January";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.8218e6, e) << "Month energy of December";
}

/// Using 30m Wind Resource File
TEST_F(CMWindPowerIntegration, DISABLED_Resource30mSimpleWake_cmod_windpower){
#ifdef _MSC_VER	
	std::string file = "../../../test/input_docs/wind_30m.srw";
#else	
	std::string file = "../test/input_docs/wind_30m.srw";
#endif
	modify_var(vartab, "wind_resource_filename", file);
	vartab->unassign("wind_resource_data");

	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 33224154, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.8218e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.8218e6, e);
	size_t nEntries = vartab->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 2);
}

/// Using 30m Wind Data
TEST_F(CMWindPowerIntegration, DISABLED_Data30mSimpleWake_cmod_windpower){
	windResourceData = create_winddata_array(2);
	vartab->unassign("wind_resource_filename");
	vartab->assign("wind_resource_data", *windResourceData);
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 33224154, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.8218e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.8218e6, e);
	size_t nEntries = vartab->lookup("gen")->num.ncols();
	EXPECT_EQ(nEntries, 8760 * 2);
}