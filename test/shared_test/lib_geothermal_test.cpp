#include "lib_geothermal_test.h"

TEST_F(CGeothermalAnalyzerBinary, Setup)
{
	EXPECT_NE(geoBinary_outputs.maf_hourly_power, nullptr);
	//EXPECT_NE(geoBinary_outputs.max_secondlaw, nullptr);
}

