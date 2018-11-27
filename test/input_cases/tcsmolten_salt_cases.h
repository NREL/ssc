#ifndef _TCSMOLTEN_SALT_CASES_H_
#define _TCSMOLTEN_SALT_CASES_H_

#include <map>
#include "code_generator_utilities.h"
#include "tcsmolten_salt_common_data.h"

/**
*   Data for high-level integration tests that verifies whether results for a molten salt power tower
*   plant in Daggett, CA matches expected results.
*   Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to tcsmolten_salt
*/
int tcsmolten_salt_daggett(ssc_data_t &data)
{
    tcsmolten_salt_default(data);
	int status = run_module(data, "tcsmolten_salt");

    //single_owner_default(data);
    //status += run_module(data, "singleowner");

	return status;
}

#endif
