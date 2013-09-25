#define __TCSTYPEINTERFACE__
#include "tcstype.h"

class weatherreader : public tcstypeinterface
{
public:
	weatherreader( tcscontext *t, int id, const char *name )
		: tcstypeinterface( t, id, name )
	{		
		std::string file = parameter_string("file_name");
		
		// must call 'configure' in type constructor to define number of required inputs, outputs
		// this is not beautiful, but it allows the number of inputs/outputs to depend on parameters,
		// which means that a type can be very flexible in different situations
		configure( 0, 10 );		
	}
	
	virtual ~weatherreader()
	{
		// close any files or free any allocated memory if needed
	}
		
	virtual int init()
	{
		return 0; // return negative numbers to indicate an error
	}
	
	virtual int call()
	{
		return 0; // return negative numbers to indicate an error
	}
};

TCS_IMPLEMENT_TYPE( weatherreader );

