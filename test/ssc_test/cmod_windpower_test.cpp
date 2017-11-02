#include <gtest\gtest.h>

#include "core.h"
#include "vartab.h"
#include "common.h"
#include "cmod_windpower.h"
#include "cmod_windpower_test.h"

// weatherfile interpolation tests

/// Using Wind Resource Data with various Wake Models
TEST_F(CMWindPowerIntegration, ResourceSimpleWake_cmod_windpower){
	clock_t Start = clock();
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 33224154, e) << "Annual energy.";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.8218e6, e) << "Monthly energy of January";
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.8218e6, e) << "Month energy of December";

}

TEST_F(CMWindPowerIntegration, ResourceWAsp_cmod_windpower){
	clock_t Start = clock();
	modify_var(vartab, "wind_farm_wake_model", 1);

	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);
	EXPECT_NEAR(ann_energy, 32346158, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.7472e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.7472e6, e);
}

TEST_F(CMWindPowerIntegration, ResourceEddy_cmod_windpower){
	clock_t Start = clock();
	modify_var(vartab, "wind_farm_wake_model", 2);

	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);

	EXPECT_NEAR(ann_energy, 31081848, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 2.6398e6, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 2.6398e6, e);

}

/// Using Weibull Distribution
TEST_F(CMWindPowerIntegration, Weibull_cmod_windpower){
	clock_t Start = clock();
	modify_var(vartab, "wind_resource_model_choice", 1);
	compute();
	float ann_energy = vartab->lookup("annual_energy")->num.at(0);

	EXPECT_NEAR(ann_energy, 180453760, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(0), 15326247, e);
	EXPECT_NEAR(vartab->lookup("monthly_energy")->num.at(11), 15326247, e);
}

/// Using 30m Wind Resource Data
TEST_F(CMWindPowerIntegration, Resource30mSimpleWake_cmod_windpower){
	clock_t Start = clock();
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

}