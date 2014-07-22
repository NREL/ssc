#include "core.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"

static var_info _cm_vtab_solarpilot[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                               UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",            "local weather file path",                     "",       "",                        "Weather",      "*",                       "LOCAL_FILE",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_size",                    "Nameplate capacity",                          "kW",     "",                        "PVWatts",      "*",                       "MIN=0.05,MAX=500000",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                         "System derate value",                         "frac",   "",                        "PVWatts",      "*",                       "MIN=0,MAX=1",                              "" },
	
	/* outputs */

	{ SSC_OUTPUT,       SSC_ARRAY,       "dn",                             "Beam irradiance",                             "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                            "Plane of array irradiance",                   "W/m2",   "",                        "PVWatts",      "*",                       "LENGTH=8760",                          "" },
	
	var_info_invalid };

static void solarpilot_callback( simulation_info *siminfo, void *data )
{

}

class cm_solarpilot : public compute_module
{
public:
	
	cm_solarpilot()
	{
		add_var_info( _cm_vtab_solarpilot );
	}

	void exec( ) throw( general_error )
	{
		AutoPilot_S sapi;

		sp_optimize opt;
		sp_ambient amb;
		sp_cost cost;
		sp_heliostats helios;
		sp_receivers recs;
		sp_layout layout;
	
		var_set V;
		ioutil::parseDefinitionArray(V);
	
		//define stuff
		opt.LoadDefaults(V);
		amb.LoadDefaults(V);
		cost.LoadDefaults(V);
		helios.resize(1);
		helios.front().LoadDefaults(V);
		recs.resize(1);
		recs.front().LoadDefaults(V);
		layout.LoadDefaults(V);

		//set up the weather data for simulation
		const char *wf = as_string("solar_resource_file" );
		if ( !wf ) throw exec_error( "solarpilot", "no weather file specified" );
		vector<string> wdata;
		wdata.push_back( std::string(wf) );
		sapi.GenerateDesignPointSimulations(amb, V, wdata);
	
		sapi.SetCallback( solarpilot_callback, (void*)this, false);
		sapi.Setup(amb, cost, layout, helios, recs);
		sapi.CreateLayout();

		//	sapi.Optimize(opt, recs, layout);
	
		sp_optical_table opttab;
		sp_flux_table fluxtab;
	
		sapi.CalculateOpticalEfficiencyTable(opttab);
		sapi.CalculateFluxMaps(fluxtab);
	}
};

DEFINE_MODULE_ENTRY( solarpilot, "SolarPILOT - CSP tower solar field layout tool.", 0 )
