#include "lib_geothermal_test.h"



TEST_F(GeothermalPlantAnalyzer, TestBinaryPlant)
{
	conversion_type = 0;	//Binary = 0 ; Flash = 1
	GeothermalPlantAnalyzer::SetUp();

	EXPECT_NEAR(geoBinary_outputs.max_secondlaw, 0.4, 0.2);
	EXPECT_NEAR(geoBinary_outputs.md_GrossPlantOutputMW, 34.787, 1.5);
	EXPECT_NEAR(geoBinary_outputs.GF_flowrate, 4993110, 200000);	
}


TEST_F(GeothermalPlantAnalyzer, TestFlashPlant) {
	conversion_type = 1;	//Binary = 0 ; Flash = 1
	GeothermalPlantAnalyzer::SetUp();

	EXPECT_GE(geoBinary_outputs.flash_count, 1);
}
