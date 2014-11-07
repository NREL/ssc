#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum {
	P_FILENAME,

	N_MAX
};

tcsvarinfo sam_type250_input_generator_variables[] = {
	/* DIRECTION    DATATYPE      INDEX       NAME           LABEL                                  UNITS      GROUP    META    DEFAULTVALUE */
	{ TCS_PARAM, TCS_STRING, P_FILENAME, "file_name", "Weather file name on local computer", "", "", "", "" },

	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};

class sam_type250_input_generator : public tcstypeinterface
{
private:
	double blah;

public:

	sam_type250_input_generator(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		blah = 1.23;
	}

	virtual ~sam_type250_input_generator() {}

	virtual int init()
	{
		blah = blah + 1.0;

		return 0;
	}

	virtual int call(double time, double step, int ncall)
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE(sam_type250_input_generator, "Input generator for Type250", "Ty Neises", 1, sam_type250_input_generator_variables, NULL, 0)