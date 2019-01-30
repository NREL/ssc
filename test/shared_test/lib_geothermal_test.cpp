#include "lib_geothermal_test.h"



TEST_F(GeothermalPlantAnalyzer, TestBinaryPlant)
{
	EXPECT_NEAR(geoBinary_outputs.max_secondlaw, 0.4, 0.2);
}


TEST_F(GeothermalPlantAnalyzer, TestFlashPlant) {
	
	
	EXPECT_GE(geoBinary_outputs.flash_count, 0);
}
