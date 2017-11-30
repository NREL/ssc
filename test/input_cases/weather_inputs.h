#ifndef _WEATHERINPUT_
#define _WEATHERINPUT_

#include "common.h"

/**
 * Creates resources as var_data, as opposed to resources from files, for testing use through SDK
 */

/// Must be deleted by calling method. 
var_data* create_weatherdata_array();

void free_weatherdata_array(var_data* data);

var_data* create_winddata_array();

void free_winddata_array(var_data* data);

#endif