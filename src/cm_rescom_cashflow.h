#ifndef __cm_rescom_cashflow_h
#define __cm_rescom_cashflow_h

#include "core.h"

class cm_rescom_cashflow : public compute_module
{
public:
	cm_rescom_cashflow( int mode /* 0=electric, 1=gas */ ) { }
	virtual bool exec( ) throw( general_error ) { return false; }
};

#endif
