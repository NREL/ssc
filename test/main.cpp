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
#if defined( _WINDOWS) && defined(_DEBUG)
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include <stdlib.h>
#include <iostream>
#include <gtest/gtest.h>

// in order to get MS V2017 update 2 to build without a bunch of C4996 "std::tr1:warning..."
#define _SILENCE_TR1_NAMESPACE_DEPRECIATION_WARNING

GTEST_API_ int main(int argc, char **argv) {

#if defined( _WINDOWS) && defined(_DEBUG)
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

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
//	::testing::GTEST_FLAG(filter) = "AAPVSmoothing_lib_battery_dispatch*";
//    ::testing::GTEST_FLAG(filter) = "sscapi_test*";
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting:CMPvwattsV7Integration_cmod_pvwattsv7.DefaultNoFinancialModel_cmod_pvwattsv7:CMPvwattsV7Integration_cmod_pvwattsv7.NonAnnual"; //works with 24 only 20 times in a row
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting";
//   ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7*";
//   ::testing::GTEST_FLAG(filter) = "Data9999CaseWeatherData*";
//	::testing::GTEST_FLAG(filter) = "CMPvsamv1PowerIntegration_cmod_pvsamv1.DefaultResidentialModel";
//	::testing::GTEST_FLAG(filter) = "BatteryPowerFlowTest_lib_battery_powerflow.TestDCConnected";
//	::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.IntermediateOutputTesting";
//	::testing::GTEST_FLAG(filter) = "CmodCashLoanTest*";
//    ::testing::GTEST_FLAG(filter) = "windpower_landbosse*";
//    ::testing::GTEST_FLAG(filter) = "save_as_JSON_test_run*";
//    ::testing::GTEST_FLAG(filter) = "csp_tower*";
//    ::testing::GTEST_FLAG(filter) = "csp_trough.PowerTroughCmod*";
//    ::testing::GTEST_FLAG(filter) = "save_as_JSON_test.pvwatts*";
//	::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8*";
//    CMPvwattsv8Integration_cmod_pvwattsv8
    //    ::testing::GTEST_FLAG(filter) = "CMPvsamv1BatteryIntegration_cmod_pvsamv1.ResidentialDCBatteryModelPriceSignalDispatch";
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8:CMPvwattsv8Integration_cmod_pvwattsv8.NonAnnual";
    //    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8:CMPvwattsv8Integration_cmod_pvwattsv8.IntermediateOutputTesting";
    // no memory leaks
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultSetup";
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8:CMPvwattsv8Integration_cmod_pvwattsv8.DifferentTechnologyInputs_cmod_pvwattsv8";


 //    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8:CMPvwattsv8Integration_cmod_pvwattsv8.SnowModelTests_cmod_pvwattsv8";
    
 //  ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8:CMPvwattsv8Integration_cmod_pvwattsv8.NonAnnual:CMPvwattsv8Integration_cmod_pvwattsv8.IntermediateOutputTesting";
    //    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.DefaultNoFinancialModel_cmod_pvwattsv8";
    //
    //
    // 
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsv8Integration_cmod_pvwattsv8.*"; //all 11 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsV7Integration_cmod_pvwattsv7.*"; //all 9 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "CMPvwattsV5Integration_cmod_pvwattsv5.*"; //all 5 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "CM_SWH.*"; //all 2 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "UsingDataCaseWeatherReader*"; //all pass with no mem leaks Windows 10
 //    ::testing::GTEST_FLAG(filter) = "CMPvsamv1PowerIntegration_cmod_pvsamv1.*"; //all 21 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "CMWindPowerIntegration.*"; //all 10 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "Turbine_powercurve_cmod_windpower_eqns.*"; //all 6 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "sscapi_test.*"; //all 4 pass without mem leaks Windows 10
//    ::testing::GTEST_FLAG(filter) = "CMPvsamv1BatteryIntegration_cmod_pvsamv1.*"; // all 20 pass with no memory leaks  windows 10 and macOS 12.0.1

 //   _CrtMemState memoryState = { 0 };
 //   _CrtMemCheckpoint(&memoryState);

    // memory leak
//     ::testing::GTEST_FLAG(filter) = "windpower_landbosse.*"; //all 4 pass with mem leaks from GoogleTest FAIL Windows 10
    //   ::testing::GTEST_FLAG(filter) = "CM_MHKWave*:CmodCashLoanTest*:CMWindPowerIntegration.*"; // 2 of 8 pass with no mem leaks in Windows 10
 //        ::testing::GTEST_FLAG(filter) = "AutoBTMTest*:CMWindPowerIntegration.*"; //all 4 pass with mem leaks from
  //  ::testing::GTEST_FLAG(filter) = "-SixParSolve_6par_solve*:UsingFileCaseWeatherReader*:UsingDataCaseWeatherReader*:LiIon_lib_battery_capacity_test*:KiBam_lib_battery_capacity_test*:AutoBTMTest_lib_battery_dispatch*:AutoFOM_lib_battery_dispatch*:ManualTest_lib_battery_dispatch*:ManualTest_lib_battery_dispatch_losses*:PVSmoothing_lib_battery_dispatch*:lib_battery_lifetime_lmolto_test*:lib_battery_lifetime_nmc_test*:lib_battery_lifetime_cycle_test*:lib_battery_lifetime_calendar_matrix_test*:lib_battery_lifetime_calendar_model_test*:lib_battery_lifetime_test*:BatteryPowerFlowTest_lib_battery_powerflow*:lib_battery_thermal_test*:lib_battery_losses_test*:lib_battery_test*:voltage_dynamic_lib_battery_voltage_test*:voltage_dynamic_lib_battery_voltage_cutoff_test*:voltage_table_lib_battery_voltage_test*:voltage_vanadium_lib_battery_voltage_test*:csp_common.StorageTank*:csp_common.TowerSharedWithUi*:csp_common.TroughSharedWithUi*:csp_trough.TroughLoop*:FuelCellTest*:GeothermalPlantAnalyzer*:NightCaseIrradProc*:SunriseCaseIrradProc*:IrradTest*:DayCaseIrradProc*:SunsetCaseIrradProc*:BifacialIrradTest*:SingleAxisTrackingTest*:ResilienceTest_lib_resilience*:sharedInverterTest_lib_shared_inverter*:solar_thermal.FlatPlateCollectorTest*:solar_thermal.FlatPlateArrayTest*:libTimeTest_lib_time*:libUtilTests*:sscapiTest*:lib_utility_rate_equations_test*:lib_utility_rate_test*:CSVCase_WeatherfileTest*:weatherfileTest*:Data8760CaseWeatherData*:Data9999CaseWeatherData*:DataSingleTimestepWeatherData*:windTurbineTest*:simpleWakeModelTest*:parkWakeModelTest*:eddyViscosityWakeModelTest*:windPowerCalculatorTest*:CMBatteryEqns_cmod_battery_eqns*:CMPvsamv1BatteryIntegration_cmod_pvsamv1*:CMBatteryStatefulIntegration_cmod_battery_stateful*:CMBattery_cmod_battery*:CMBattwatts_cmod_battwatts*:CMBiomass*:CmodCashLoanTest*:CMFuelCell*:CMGeneric*:CMGeothermalCosts*:CMGrid_cmod_grid*:CM_MHKTidal*:CM_MHKWave*:CMPvsamv1PowerIntegration_cmod_pvsamv1*:CMPvwattsV5Integration_cmod_pvwattsv5*:CMPvwattsV7Integration_cmod_pvwattsv7*:CMPvwattsv8Integration_cmod_pvwattsv8*:CMPvYieldTimo*:CMSingleOwner*:CM_SWH*:windDataProviderCalculatorTest*:CMGeothermal*:csp_fresnel.PowerFresnelCmod*:csp_tower.PowerTowerCmod*:csp_trough.EmpiricalTroughCmod*:csp_trough.HeatTroughCmod*:csp_trough.PowerTroughCmod*:URDBv7_cmod_utilityrate5_eqns*:cmod_utilityrate5_eqns*:Wfreader_cmod_wfreader*:CMWindPowerIntegration*:Turbine_powercurve_cmod_windpower_eqns*:windpower_landbosse*";
        
//    ::testing::GTEST_FLAG(filter) = "lib_battery_test.*"; // all 20 pass with no memory leaks  windows 10 and macOS 12.0.1
    ::testing::GTEST_FLAG(filter) = "-CMGeothermal*:GeothermalPlantAnalyzer*";
     int status = RUN_ALL_TESTS();

/*
#if defined( _WINDOWS) && defined(_DEBUG)
    _CrtMemDumpAllObjectsSince(&memoryState);
#endif
*/
	if (!status)
		printf("Tests Pass!\n");
	return status;
}
