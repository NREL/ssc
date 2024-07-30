/********************************************************************************************************************************************

Note
-------------
The version of nlopt included in this repository has been modified as follows:
1. The original .c files have been modified to .cpp files to facilitate the use of c++ std library functions for abs, fabs, sqrt, etc.
2. The nlopt specific file modifications can be found at https://github.com/NREL/ssc/commits/patch/nlopt

The original version of nlopt can be found at https://github.com/stevengj/nlopt

********************************************************************************************************************************************/
#ifndef __PLATFORM_CONFIG__
#define __PLATFORM_CONFIG__

// Due to problems with the NLOPT config script on windows, we've
// set up a file which will use our correctly windows config script
// Or, in the event of Linux/Mac, use a generated config.h file
// Modified 3/25/2019
#ifdef _WIN32
#include "config-windows.h"
#else
#include "config.h"
#endif 

#endif
