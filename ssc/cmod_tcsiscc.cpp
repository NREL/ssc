#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsiscc[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                        "",             "",             "Weather",        "*",                       "LOCAL_FILE",            "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Ambient dry bulb temperature",                                   "C",            "",             "Outputs",        "*",                       "LENGTH=8760",           "" },

	var_info_invalid };

class cm_tcsiscc : public tcKernel
{
public:

	cm_tcsiscc(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsiscc );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		//Add weather file reader unit
		int weather = add_unit("weatherreader", "TCS weather reader");

		//Set weatherreader parameters
		set_unit_value_ssc_string( weather, "file_name" );

		bool bConnected = true;

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcs_iscc", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcs_iscc", util::format("there was a problem simulating in tcs_iscc.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcs_iscc", util::format("there was a problem returning the results from the simulation.") );

	}

};

DEFINE_TCS_MODULE_ENTRY( tcsiscc, "Triple pressure NGCC integrated with MS power tower", 4 )
