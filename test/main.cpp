/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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
//    ::testing::GTEST_FLAG(filter) = "save_as_JSON_test_parse*";
//    ::testing::GTEST_FLAG(filter) = "save_as_JSON_test*";
//    ::testing::GTEST_FLAG(filter) = "save_as_JSON_test_run*";
//    ::testing::GTEST_FLAG(filter) = "sscapi_test*";
    /*
    CMPvwattsV7Integration_cmod_pvwattsv7.NonAnnual
    CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting
    */
//    ::testing::GTEST_FLAG(filter) = "BatteryPowerFlowTest_lib_battery_powerflow.TestDCConnected";



	int status = RUN_ALL_TESTS();

	if (!status)
		printf("Tests Pass!\n");
	return status;
}
