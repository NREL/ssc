#ifndef __COMPUTE_MODULE_TEST__
#define __COMPUTE_MODULE_TEST__ 

#include <gtest/gtest.h>
#include <string>


#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"

#include "simulation_test_info.h"

namespace {
	using ::testing::TestWithParam;
	//typedef SimulationTestTable* CreateSimulationTestTableFunc();

}

class computeModuleTest : public TestWithParam<SimulationTestTable*> {
	const char* errorMsg;

public:
	const char* getErrorMsg() { return errorMsg; }
	void SetUp() {
		table_ = GetParam();
		data_ = ssc_data_create();
		ssc_module_exec_set_print(0);
		EXPECT_TRUE(testTableToSSCData());
	}
	void TearDown() {
		ssc_data_free(data_);
	}
	
	/// runs compute module and returns true if successful
	bool compute() {
		ssc_module_t module = ssc_module_create(table_->getCMODType().c_str());
		if (NULL == module)
		{
			printf("error: could not create %s module.", table_->getCMODType().c_str());
			return false;
		}
		if (ssc_module_exec(module, data_) == 0)
		{
			compute_module *cm = static_cast<compute_module*>(module);
			printf("%s - %s did not compute: \n", table_->getCMODType().c_str(), table_->name.c_str());

			int i = 0;
			while (cm->log(i) != nullptr) {
				printf("%s\n", cm->log(i)->text.c_str());
				i++;
			}
			ssc_module_free(module);
			return false;
		}
		ssc_module_free(module);
		return true;
	}

protected:
	SimulationTestTable * table_;
	ssc_data_t data_;

private:
	/// Takes the input values in TestInfo to assign variables in ssc_data_t
	bool testTableToSSCData() {
		for (int i = 0; i < table_->getNumInfo(); i++) {
			const TestInfo* info = &(table_->getInfo())[i];
			if (info->dataType == STR) {
				ssc_data_set_string(data_, info->sscVarName.c_str(), info->values.c_str());
			}
			else if (info->dataType == NUM) {
				ssc_data_set_number(data_, info->sscVarName.c_str(), (ssc_number_t)atof(info->values.c_str()));
			}
			else if (info->dataType == ARR) {
				size_t n = info->length;
				std::stringstream ss(info->values);
				ssc_number_t* val = new ssc_number_t[n]();
				for (size_t j = 0; j < n; j++) {
					std::string substr;
					getline(ss, substr, ',');
					ssc_number_t value = (ssc_number_t)atof(substr.c_str());
					val[j] = value;
				}
				ssc_data_set_array(data_, info->sscVarName.c_str(), val, (int)n);
			}
			else if (info->dataType == MAT) {
				size_t n = info->length;
				size_t m = info->width;
				std::stringstream ss(info->values);
				ssc_number_t* val = new ssc_number_t[n*m]();
				for (size_t j = 0; j < n*m; j++) {
					std::string substr;
					getline(ss, substr, ',');
					val[j] = (ssc_number_t)atof(substr.c_str());
				}
				ssc_data_set_matrix(data_, info->sscVarName.c_str(), val, (int)n, (int)m);
			}
			else {
				return false;
			}
		}
		return true;
	}
};


#endif