#ifndef _WEATHERINPUT_
#define _WEATHERINPUT_

#include "common.h"

/**
 * This header declares data structures for making test cases which require a lot of input.
 * Put data to be tested in this file that may be useful in tests of several different classes
 * and to minimize recompiliation of large, constant functions.
 */

/// Must be deleted by calling method. 
var_data* create_weatherdata_array();

void free_weatherdata_array(var_data* data);

var_data* create_winddata_array();

void free_winddata_array(var_data* data);

#endif