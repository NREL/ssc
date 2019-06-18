#include <stdlib.h>
#include <iostream>
#include <gtest/gtest.h>

// in order to get MS V2017 update 2 to build without a bunch of C4996 "std::tr1:warning..."
#define _SILENCE_TR1_NAMESPACE_DEPRECIATION_WARNING

GTEST_API_ int main(int argc, char **argv) {


	printf("Running main() from gtest_main.cc\n");
	testing::InitGoogleTest(&argc, argv);
//	  ::testing::GTEST_FLAG(filter) = "CMPvYieldTimo*";
//	  ::testing::GTEST_FLAG(filter) = "splinterTests*";
//	  ::testing::GTEST_FLAG(filter) = "SunsetCaseIrradProc*";
//	  ::testing::GTEST_FLAG(filter) = "BatteryPowerFlowTest*";
//	  ::testing::GTEST_FLAG(filter) = "CMGeneric*";

	int status = RUN_ALL_TESTS();

	if (!status)
		printf("Tests Pass!\n");
	return status;
}
