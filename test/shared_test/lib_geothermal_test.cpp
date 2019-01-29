#include "lib_geothermal_test.h"

TEST_F(GeothermalPlantAnalyzer, TestBinaryPlant)
{
	EXPECT_NE(geoBinary_outputs.maf_hourly_power, nullptr);
	EXPECT_NE(geoBinary_outputs.max_secondlaw, NULL);
}


TEST_F(GeothermalPlantAnalyzer, TestFlashPlant) {
	
	
	EXPECT_GE(geoBinary_outputs.flash_count, 0);
}
