#include "core.h"

static var_info vtab_iph_to_lcoefcr[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                             UNITS     META      GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,       SSC_NUMBER,      "annual_energy_MWt",  "Annual Field Thermal Energy Production w/ avail derate",     "MWt-hr", "",          "Post-process",     "*",       "",   "" },
	
	{ SSC_OUTPUT,       SSC_NUMBER,     "annual_energy",      "Annual energy production",       "kWt-hr/yr", "",       "Simple LCOE", "*",           "",         "" },


var_info_invalid };

class cm_iph_to_lcoefcr : public compute_module
{
private:
public:
	
	cm_iph_to_lcoefcr()
	{
		add_var_info( vtab_iph_to_lcoefcr );
	}

	void exec( ) throw( general_error )
	{
	
		assign("annual_energy", as_double("annual_energy_MWt")*1.E3);
	}
	
};

DEFINE_MODULE_ENTRY( iph_to_lcoefcr, "Convert annual energy to kWt-hr and adjust fixed cost to include electric parasitic costs.", 1 )
