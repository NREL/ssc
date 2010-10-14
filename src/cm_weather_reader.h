#ifndef __cm_weather_reader_h
#define __cm_weather_reader_h

#include "core.h"

class cm_weather_reader : public compute_module
{
public:
	cm_weather_reader() { }

	virtual bool exec( ) throw( general_error ) { return false; }
};

#endif

