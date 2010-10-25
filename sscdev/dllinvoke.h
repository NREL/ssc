#ifndef __dllinvoke_h
#define __dllinvoke_h


#include <sscapi.h>

bool sscdll_load( const char *path );
void sscdll_unload();
bool sscdll_isloaded();
const char *sscdll_status();

#endif
