#include "core.h"


extern var_info vtab_utility_rate[];

class cm_utilityrate : public compute_module
{
private:
public:
	cm_utilityrate()
	{
		add_var_info( vtab_utility_rate );
	}

	void exec( ) throw( general_error )
	{
	}
};

DEFINE_MODULE_ENTRY( utilityrate, "Complex utility rate structure calculator_", 1 );


