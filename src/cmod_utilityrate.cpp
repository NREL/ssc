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

	bool exec( ) throw( general_error )
	{
		return false;
	}
};

DEFINE_MODULE_ENTRY( utilityrate, "Complex utility rate structure calculator_", 1 );


