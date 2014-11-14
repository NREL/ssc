#include "core.h"

static var_info _cm_vtab_dsg_flux_preprocess[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                   UNITS     META  GROUP REQUIRED_IF         CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "P_HP_in",  "HP Turbine inlet pressure",        "bar",    "",  "",  "*", "", "" },

	{ SSC_INPUT, SSC_STRING, "solar_resource_file", "Solar weather data file", "", "", "SolarPILOT", "*", "LOCAL_FILE", "" },

	var_info_invalid };

class cm_dsg_flux_preprocess : public compute_module
{
public:

	cm_dsg_flux_preprocess()
	{
		add_var_info(_cm_vtab_dsg_flux_preprocess);
	}

	void exec( ) throw( general_error )
	{
	
	
	}


};

DEFINE_MODULE_ENTRY( dsg_flux_preprocess, "Calculate receiver max flux and absorber (boiler, etc.) fractions", 0)