#ifndef _PVWATTSV5_CASES_
#define _PVWATTSV5_CASES_

#include <stdio.h>
#include <string>
#include "input_cases/code_generator_utilities.h"

int pvwattsv5_nofinancial_testfile(ssc_data_t &data)
{
	//this gets included every time
	ssc_module_exec_set_print(0);
	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}

	//this is so that it can run on different operating systems, change weather file name to match
#ifdef _MSC_VER	
	std::string file = "../../../test/input_docs/weather.csv";
#else	
	std::string file = "../test/input_docs/weather.csv";
#endif	



}


#endif



