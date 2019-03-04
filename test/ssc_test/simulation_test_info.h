#ifndef __TestInfo__
#define __TestInfo__
#include <unordered_map>
#include <iostream>
#include <vector>
#include <string>

enum {
	STR,
	NUM,
	ARR,
	MAT
};

struct TestInfo{
	std::string sscVarName;		///< same name as SSC variable
	unsigned int dataType;		///< 0: string, 1: number, 2: array, 3: matrix

	std::string values;			///< comma-separated for array of values
	size_t length = 1;			///< length of array/matrix
	size_t width = 1;			///< width of matrix

	TestInfo(std::string varName, unsigned int type, std::string val, size_t len = 1, size_t wid = 1)
	{
		sscVarName = varName;
		dataType = type;
		values = val;
		length = len; 
		width = wid;
	}

	TestInfo() {}
};

// enum for test_types: equal, near (approx equal), greater than, less than, bool, cmod error
enum {
	EQ,
	NR,
	GT,
	LT,
	TF,
	ERR
};

struct TestResult {
	//TestInfo* test_values;

	std::string sscVarName;
	unsigned int testType;	
	double expectedResult;		///< expected test results per value
	double errorBound;			///< percent error allowed
};


class SimulationTestTable {
public:
	std::string name;
	SimulationTestTable(std::string cmodType, std::string testName, TestInfo* I, int nInfo, TestResult* R, int nRes) {
		name = testName;
		computeModuleType = cmodType;
		info = I;
		result = R;
		nI = nInfo;
		nR = nRes;
	}
	// to add const
	SimulationTestTable(SimulationTestTable& first) {
		name = first.name;
		computeModuleType = first.computeModuleType;
		nI = first.nI;
		nR = first.nR;
		info = new TestInfo[nI];
		result = new TestResult[nR];
		for (int i = 0; i < nI; i++) {
			info[i].dataType = first.getInfo()[i].dataType;
			info[i].length = first.getInfo()[i].length;
			info[i].sscVarName = first.getInfo()[i].sscVarName;
			info[i].values = first.getInfo()[i].values;
			info[i].width = first.getInfo()[i].width;

		}
		for (int i = 0; i < nR; i++) {
			result[i].errorBound = first.getResult()[i].errorBound;
			result[i].expectedResult = first.getResult()[i].expectedResult;
			result[i].sscVarName = first.getResult()[i].sscVarName;
			result[i].testType = first.getResult()[i].testType;
		}

	}
	//to add dtor
	std::string getCMODType() { return computeModuleType; }
	int getNumInfo() { return nI; }
	int getNumResult() { return nR; }
	TestInfo* getInfo() { return info; }
	TestResult* getResult() { return result; }
	void setResult(TestResult* R, size_t nRes) { result = R; nR = (int)nRes; }

	SimulationTestTable& operator=(SimulationTestTable other) {
		swap(other);
		return *this;
	}

	bool modifyTestInfo(std::unordered_map<std::string, size_t>& map, TestInfo* I, size_t nInfo, std::string name) {
		for (size_t i = 0; i < nInfo; i++) {
			size_t varIndex = 0;
			std::unordered_map<std::string, size_t>::iterator it = map.find(I[i].sscVarName);
			if (it != map.end()) {
				varIndex = (*it).second;
				info[varIndex].values = I[i].values;
			}
			else {
				std::cout << computeModuleType << "-" << name << ": could not find sscVarName "
					<< I[i].sscVarName << "\n";
				return false;
			}
		}
		return true;
	}

protected:
	std::string computeModuleType;
	TestInfo* info;
	TestResult* result;
	int nI, nR;

	void swap(SimulationTestTable& other) {
		using std::swap;
		SimulationTestTable first = *this;
		swap(first.name, other.name);
		swap(first.computeModuleType, other.computeModuleType);
		swap(first.info, other.info);
		swap(first.result, other.result);
		swap(first.nI, other.nI);
		swap(first.nR, other.nR);
	}
};

class computeModuleTestData {
public:
	std::vector<SimulationTestTable*>* tests;
	std::unordered_map<std::string, size_t>* map;
	std::string name;

	computeModuleTestData(std::vector<SimulationTestTable*>* t, std::unordered_map<std::string, size_t>* m, std::string n) {
		tests = t;
		map = m;
		name = n;
	}
};

/**
* testDeclaration creates a SimulationTestTable out of a pair of TestInfo and TestResult structs.
* The new SimulationTestTable is added to given vector, to be instantiated as computeModuleTests.
* Modifying TestInfo is created from duplicated default TestInfo.
*/

class testDeclaration {
	SimulationTestTable* test;
public:
	testDeclaration(computeModuleTestData& testData, const char* testName, TestInfo* I, size_t nInfo, TestResult* R, int nRes) {
		std::vector<SimulationTestTable*>* allTests = testData.tests;
		if (allTests->size() > 0 && (int)nInfo < (*allTests)[0]->getNumInfo()) {
			test = new SimulationTestTable(*(*allTests)[0]);
			test->name = testName;
			bool success = test->modifyTestInfo(*(testData.map), I, nInfo, testName);
			if (success) {
				test->setResult(R, nRes);
				(*allTests).push_back(test);
			}
		}
		else {
			for (size_t i = 0; i < nInfo; i++) {
				(*testData.map)[I[i].sscVarName] = i;
			}
			test = new SimulationTestTable(testData.name, testName, I, (int)nInfo, R, nRes);
			(*allTests).push_back(test);
		}
	}
	~testDeclaration() {
		delete test;
	}
};


#endif
