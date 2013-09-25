#include <math.h>

#include "core.h"
//#include "lib_util.h"


static var_info _cm_vtab_test_mod[] = {
/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ TCS_INPUT,        TCS_STRING,      "file_name",             "local weather file path",          "",       "",                      "Weather",      "*",                       "LOCAL_FILE",                     "" },
	{ TCS_INPUT,        TCS_ARRAY,       "input_array",           "Input array",                      "kg/hr",  "",                      "Test",         "*",                       "",                               "" },
	{ TCS_INPUT,        TCS_NUMBER,      "input_number",          "Input number",                     "",       "",                      "Test",         "?=100",                   "MIN=0,MAX=1000",                 "" },
	
	{ TCS_OUTPUT,       TCS_ARRAY,       "out_array1",            "Operation mode",                   "",      "1,2,3,4",                "Test",         "*",                       "LENGTH_EQUAL=input_array",       "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "out_array2",            "Number of iterations to solve",    "",      "",                       "Test",         "*",                       "LENGTH=8760",                    "" },
	
	var_info_invalid };

class cm_test_mod : public compute_module
{
public:
	
	cm_test_mod()
	{
		add_var_info( _cm_vtab_test_mod );
	}

	void exec( ) throw( general_error )
	{
		// Load type library
		if ( 0 >= m_kern.load_library("typelib") )
			throw exec_error( "test_mod", util::format("could not load the tcs type library.") );

		// Create units necessary for this technology/model
		int iSumProdID = iSumProdID = m_kern.add_unit( "sumprod", "Sumprod Unit");
		if ( 0 > iSumProdID )
			throw exec_error( "test_mod", util::format("could not find the 'sumprod' type in the tcs library.") );

		int iwf = iwf = m_kern.add_unit( "weatherreader", "Weather File Reader Unit");
		if ( 0 > iwf )
			throw exec_error( "test_mod", util::format("could not find the 'weatherreader' type in the tcs library.") );

		// Set unit starting values
		m_kern.set_unit_value(iSumProdID, "a", 0.0 );
		m_kern.set_unit_value(iSumProdID, "b", as_double("input_number"));
		m_kern.set_unit_value(iwf, "file_name", as_string("file_name"));


		// Connect units
		bool bConnected = m_kern.connect(iwf, "wspd", iSumProdID, "a");
		bConnected &= m_kern.connect(iwf, "wdir", iSumProdID, "b");
		if ( !bConnected )
			throw exec_error( "test_mod", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );


		// Run simulation




		double f1 = as_double("input_number");

		size_t len=0;
		tcs_number_t *i1 = as_array("input_array", &len);

		tcs_number_t *o1 = allocate("out_array1", len);
		for (int i=0; i<len; i++)
			o1[i] = i1[i] + 2.0;

		len = 8760;
		tcs_number_t *o2 = allocate("out_array2", len);
		for (int i=0; i<len; i++)
			o2[i] = (float)i/100.0;

	} // exec
};

DEFINE_MODULE_ENTRY( test_mod, "Test module for tcs interop api.", 216 )
