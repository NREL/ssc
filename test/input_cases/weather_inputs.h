#include "common.h"

/**
 * This header declares data structures for making test cases which require a lot of input.
 * Put data to be tested in this file that may be useful in tests of several different classes
 * and to minimize recompiliation of large, constant functions.
 */

/// Must be deleted by calling method. Data same as first five data rows in weather.csv
var_data* create_weatherdata_array();