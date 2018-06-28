#include <gtest/gtest.h>
#include <memory>
#include <vector>
#include <unordered_map>
#include "simulation_test_info.h"
#include "computeModuleTest.h"

// first test must be contain all possible inputs
std::vector<SimulationTestTable*> pvWattsIntgTests;
std::unordered_map<std::string, size_t> pvWattsIntgVarMap;
computeModuleTestData pvWattsTesting(&pvWattsIntgTests, &pvWattsIntgVarMap, "pvwattsv5");

// Set up solar resource file paths
char hourly[150];
int a = sprintf(hourly, "%s/test/input_cases/pvsamv1_data/USA AZ Phoenix (TMY2).csv", std::getenv("SSCDIR"));


/*
* Test 1
* Default Test Info: using SAM GUI defaults with TMY2
*/

TestInfo pvWattsDefaultInfo[] = {
/*	SSC Var Name							Data Type			Test Values				Length,Width */
	{"solar_resource_file",					STR,				hourly },
	{"system_capacity",						NUM,				"4"},
	{"module_type",							NUM,				"0"},
	{"dc_ac_ratio",							NUM,				"1.2"},
	{"inv_eff",								NUM,				"96"},
	{"losses",								NUM,				"14.075660705566406"},
	{"array_type",							NUM,				"0"},
	{"tilt",								NUM,				"20"},
	{"azimuth",								NUM,				"180"},
	{"gcr",									NUM,				"0.4"},
	{"adjust:constant",						NUM,				"0"}
};

TestResult pvWattsDefaultResult[] = {
/*	SSC Var Name							Test Type			Test Result				Error Bound Ratio */
	{"annual_energy",						NR,					6909.79,				0.1},
	{"monthly_energy[0]",					NR,					435.384,				0.1},
	{"monthly_energy[1]",					NR,					482.864,				0.1},
	{"monthly_energy[2]",					NR,					593.982,				0.1},
	{"monthly_energy[3]",					NR,					673.599,				0.1},
	{"monthly_energy[4]",					NR,					715.839,				0.1},
	{"monthly_energy[5]",					NR,					665.064,				0.1},
	{"monthly_energy[6]",					NR,					665.71,					0.1},
	{"monthly_energy[7]",					NR,					647.677,				0.1},
	{"monthly_energy[8]",					NR,					594.505,				0.1},
	{"monthly_energy[9]",					NR,					568.489,				0.1},
	{"monthly_energy[10]",					NR,					453.529,				0.1},
	{"monthly_energy[11]",					NR,					413.149,				0.1},
	{"capacity_factor",						NR,					19.7197,				0.1}
};

testDeclaration pvWattsDefaultTest(pvWattsTesting, "default", &pvWattsDefaultInfo[0], 11, &pvWattsDefaultResult[0], 14);

INSTANTIATE_TEST_CASE_P(pvWattsIntegrationTest, computeModuleTest, testing::ValuesIn(pvWattsIntgTests));