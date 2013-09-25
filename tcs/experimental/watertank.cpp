#define __TCSTYPEINTERFACE__
#include "tcstype.h"

class watertank : public tcstypeinterface
{
public:

	double currentTankTemp;
	
	class node
	{
		double temp;
		double insulation;
	};
	
	std::vector<node> m_nodes;
	
	enum { TEMP, PRES, MDOT, MAX_INPUTS };
	watertank( tcscontext *t, int id, const char *name )
		: tcstypeinterface( t, id, name )
	{		
		int nnodes = parameter("nodes");
		
		// must call 'configure' in type constructor to define number of required inputs, outputs
		// this is not beautiful, but it allows the number of inputs/outputs to depend on parameters,
		// which means that a type can be very flexible in different situations
		configure( MAX_INPUTS , nnodes );
		m_nodes.resize(10);
		
	}
	
	virtual ~watertank()
	{
		// close any files or free any allocated memory if needed
	}
		
	virtual int init()
	{	
		currentTankTemp = in(TEMPIN)
		return 0; // return negative numbers to indicate an error
	}
	
	virtual int call()
	{
	
		return 0; // return negative numbers to indicate an error
		
		for (i=0;i<10;i++)
			{ m_nodes[i].temp = x };
			
	}
};

TCS_IMPLEMENT_TYPE( watertank );

