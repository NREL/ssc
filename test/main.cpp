#include <stdlib.h>
#include <iostream>
#include <gtest/gtest.h>

// in order to get MS V2017 update 2 to build without a bunch of C4996 "std::tr1:warning..."
#define _SILENCE_TR1_NAMESPACE_DEPRECIATION_WARNING

GTEST_API_ int main(int argc, char **argv) {


	printf("Running main() from gtest_main.cc\n");
	testing::InitGoogleTest(&argc, argv);
//	::testing::GTEST_FLAG(filter) = "CMTcsMoltenSalt*";
//	::testing::GTEST_FLAG(filter) = "CMTroughPhysical*";
//	  ::testing::GTEST_FLAG(filter) = "CMPvYieldTimo*";
//	  ::testing::GTEST_FLAG(filter) = "splinterTests*";
//	  ::testing::GTEST_FLAG(filter) = "SunsetCaseIrradProc*";
//	  ::testing::GTEST_FLAG(filter) = "BatteryPowerFlowTest*";
//	::testing::GTEST_FLAG(filter) = "CMGeneric*";
//	::testing::GTEST_FLAG(filter) = "CMGeothermal*";
//		::testing::GTEST_FLAG(filter) = "CMPvsamv1PowerIntegration.NoFinancialModelShading";
//	::testing::GTEST_FLAG(filter) = "CMPvwattsV5Integration.DifferentTechnologyInputs";
//	::testing::GTEST_FLAG(filter) = "CMGeneric.SingleOwnerWithBattery_cmod_generic";
//	::testing::GTEST_FLAG(filter) = "CM_MHK*";
//	::testing::GTEST_FLAG(filter) = "CMSingleOwner*";
//	::testing::GTEST_FLAG(filter) = "BifacialIrradTest*";
//	::testing::GTEST_FLAG(filter) = "FuelCellTest*";
//	::testing::GTEST_FLAG(filter) = "CMWindPowerIntegration*";
//	::testing::GTEST_FLAG(filter) = "CMGrid*";
//	::testing::GTEST_FLAG(filter) = "CMPvsamv1PowerIntegration_cmod_pvsamv1.DefaultNoFinancialModel";
//	::testing::GTEST_FLAG(filter) = "CMPvsamv1PowerIntegration_cmod_pvsamv1.NoFinancialModelCustomWeatherFile";
//	::testing::GTEST_FLAG(filter) = "URDBv7*";
//	::testing::GTEST_FLAG(filter) = "PVSmoothing_lib_battery_dispatch*";
//    ::testing::GTEST_FLAG(filter) = "CM_SWH*";
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting:CMPvwattsV7Integration_cmod_pvwattsv7.DefaultNoFinancialModel_cmod_pvwattsv7:CMPvwattsV7Integration_cmod_pvwattsv7.NonAnnual"; //works with 24 only 20 times in a row
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting";
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7*";
//   ::testing::GTEST_FLAG(filter) = "Data9999CaseWeatherData*";



	int status = RUN_ALL_TESTS();

	if (!status)
		printf("Tests Pass!\n");
	return status;
}
