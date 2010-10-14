#ifndef __cm_pvwatts_h
#define __cm_pvwatts_h

#include "core.h"

class cm_pvwatts : public compute_module
{
public:
	cm_pvwatts() { }
	virtual bool exec( ) throw( general_error ) { return false; }
};

#endif

