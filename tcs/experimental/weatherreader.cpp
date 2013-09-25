#define _TCSTYPEINTERFACE_
#include "../tcstype.h"


enum {	I_FILENAME, 

		O_GLOBAL, 
		O_BEAM, 
		O_DIFFUSE,

		N_MAX };

tcsvarinfo weatherreader_variables[] = {

	{ TCS_INPUT,   TCS_STRING,   I_FILENAME, "file_name",   "Weather file name on local computer",  "",        "",      "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_BEAM,     "beam",        "Beam normal irradiance",               "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DIFFUSE,  "diff",        "Diffuse horizontal irradiance",        "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_GLOBAL,   "global",      "Global horizontal irradiance",         "W/m2",    "Solar", "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class weatherreader : public tcstypeinterface
{
private:
	FILE *m_fp;
public:
	weatherreader( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		m_fp = 0;
	}

	virtual ~weatherreader()
	{
		if ( m_fp != 0 ) fclose( m_fp );
	}

	virtual int init()
	{
		const char *file = value_str( I_FILENAME ).c_str();
		m_fp = fopen( file, "r" );
		if ( m_fp == 0 )
		{
			message("could not open %s for reading", file );
			return -1;
		}

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{
		value( O_BEAM, 0.0 );
		value( O_DIFFUSE, 1 );
		value( O_GLOBAL, 48 );
		return 0;
	}
};

TCS_IMPLEMENT_TYPE( weatherreader, "Standard Weather File format reader", "Aron Dobos", 1, weatherreader_variables, 0 )
