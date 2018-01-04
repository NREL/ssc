#ifndef _PVSAMV1_DATA_H_
#define _PVSAMV1_DATA_H_

#include "code_generator_utilities.h"
#include "pvsamv1_common_data.h"

/**
*   Data for high-level integration test that verifies whether results for a no-financials PV system in Phoenix
*   matches expected results.  Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to PVSAMV1
*/
int pvsam_nofinancial_pheonix(ssc_data_t &data)
{
	ssc_module_exec_set_print(0);
	pvsamv_nofinancial_default(data);
	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}
	ssc_module_t module;
	module = ssc_module_create("pvsamv1");
	if (NULL == module)
	{
		printf("error: could not create 'pvsamv1' module.");
		ssc_data_free(data);
		return -1;
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation.");
		ssc_module_free(module);
		ssc_data_free(data);
		return -1;
	}
	ssc_module_free(module);
	return 0;
}

/**
*   Data for high-level integration test that verifies whether results for a residential PV system in Phoenix
*   matches expected results.  Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to PVSAMV1
*/
int pvsam_residential_pheonix(ssc_data_t &data)
{
	ssc_module_exec_set_print(0);
	belpe_default(data);
	pvsamv1_with_residential_default(data);
	utility_rate5_default(data);
	cashloan_default(data);
	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}
	ssc_module_t module;
	module = ssc_module_create("belpe"); 
	if (NULL == module)
	{
		printf("error: could not create 'belpe' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	
	module = ssc_module_create("pvsamv1"); 
	if (NULL == module)
	{
		printf("error: could not create 'pvsamv1' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	
	module = ssc_module_create("utilityrate5"); 
	if (NULL == module)
	{
		printf("error: could not create 'utilityrate5' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	
	module = ssc_module_create("cashloan"); 
	if (NULL == module)
	{
		printf("error: could not create 'cashloan' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	
	return 0;
}

/**
*   Data for high-level integration test that verifies whether results for a no-financials PV system in Phoenix
*   with a custom input weather datafile matches expected results. 
*/
int pvsam_nofinancial_custom_input_weather(ssc_data_t &data)
{
	ssc_module_exec_set_print(0);
	pvsamv_nofinancial_default(data);
	ssc_data_set_string(data, "solar_resource_file", solar_resource_path_15_min);

	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}
	ssc_module_t module;
	module = ssc_module_create("pvsamv1");
	if (NULL == module)
	{
		printf("error: could not create 'pvsamv1' module.");
		ssc_data_free(data);
		return -1;
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation.");
		ssc_module_free(module);
		ssc_data_free(data);
		return -1;
	}
	ssc_module_free(module);
	return 0;
}

/**
*   Data for high-level integration test that verifies sky model behavior
*   for a default case 
*/
int pvsam_test_albedo_and_radiation(ssc_data_t &data, int sky_diffuse_model, int irrad_mode)
{
	ssc_module_exec_set_print(0);
	pvsamv_nofinancial_default(data);

	ssc_data_set_number(data, "irrad_mode", irrad_mode);
	ssc_data_set_number(data, "sky_model", sky_diffuse_model);

	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}
	ssc_module_t module;
	module = ssc_module_create("pvsamv1");
	if (NULL == module)
	{
		printf("error: could not create 'pvsamv1' module.");
		ssc_data_free(data);
		return -1;
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation.");
		ssc_module_free(module);
		ssc_data_free(data);
		return -1;
	}
	ssc_module_free(module);
	return 0;
}


#endif
