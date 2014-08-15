#include "core.h"
#include "lib_weatherfile.h"
#include "lib_util.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"


static var_info _cm_vtab_solarpilot[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                          UNITS     META        GROUP          REQUIRED_IF         CONSTRAINTS         UI_HINTS*/

	/*
	{ SSC_INPUT,        SSC_NUMBER,      "optimize",                  "Enable constrained optimization",            "0/1",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_tht_min",             "Tower height, minimum",                      "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_tht_max",             "Tower height, maximum",                      "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_aspect_min",      "Receiver aspect ratio, minimum",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_aspect_max",      "Receiver aspect ratio, maximum",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_height_min",      "Receiver height, minimum",                   "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_height_max",      "Receiver height, maximum",                   "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "flux_max",                  "Maximum flux",                               "kW/m2",  "",         "SolarPILOT",   "*",                "",                "" },
	*/

	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",       "Solar weather data file",                    "",       "",         "SolarPILOT",   "*",                "LOCAL_FILE",      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "width",                     "Heliostat width",                            "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "height",                    "Heliostat height",                           "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "optical_error",             "Optical error",                              "rad",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "active_fraction",           "Active fraction of reflective area",         "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "reflectance",               "Mirror reflectance",                         "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "absorptance",               "Absorptance",                                "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_hl_perm2",                "Heat loss",                                  "kW/m2",  "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_design",                  "Receiver thermal design power",              "MW",     "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_max",                  "Max heliostat-dist-to-tower-height ratio",   "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_min",                  "Min heliostat-dist-to-tower-height ratio",   "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "h_tower",                   "Tower height",                               "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_fixed_cost",          "Tower fixed cost",                           "$",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_exp",                 "Tower cost scaling exponent",                "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_cost",              "Receiver reference cost",                    "$",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_area",              "Receiver reference area for cost scale",     "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_cost_exp",              "Receiver cost scaling exponent",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_spec_cost",            "Site improvement cost",                      "$/m2",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_spec_cost",       "Heliostat field cost",                       "$/m2",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "plant_spec_cost",           "Power cycle and BOS cost",                   "$/kWe",  "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tes_spec_cost",             "Thermal energy storage cost",                "$/kWht", "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_spec_cost",            "Total land area cost",                       "$/acre", "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency_rate",          "Contingency for cost overrun",               "%",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",            "Sales tax rate",                             "%",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_frac",            "Percent of cost to which sales tax applies", "%",      "",         "SolarPILOT",   "*",                "",                "" },

	/* outputs */
	{ SSC_OUTPUT,       SSC_ARRAY,       "opteff_zeniths",            "Optical efficiency table zenith angles",     "deg",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "opteff_azimuths",           "Optical efficiency table azimuth angles",    "deg",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "opteff_table",              "Optical efficiency (azi x zen)",             "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	
	var_info_invalid };

static bool solarpilot_callback( simulation_info *siminfo, void *data );

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

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
	
		// define stuff and load default values
		opt.LoadDefaults(V);
		amb.LoadDefaults(V);
		cost.LoadDefaults(V);
		helios.resize(1);
		helios.front().LoadDefaults(V);
		recs.resize(1);
		recs.front().LoadDefaults(V);
		layout.LoadDefaults(V);

		// read inputs from SSC module
		/*
		bool isopt = as_boolean( "optimize" );
		opt.is_optimize_rec_aspect = isopt;
		opt.is_optimize_bound = isopt;
		opt.is_optimize_rec_height = isopt;
		opt.is_optimize_tht = isopt;
		opt.is_range_constr_aspect = isopt;
		opt.is_range_constr_bound = isopt;
		opt.is_range_constr_rech = isopt;
		opt.is_range_constr_tht = isopt;
		opt.range_tht[0] = as_double("range_tht_min");
		opt.range_tht[1] = as_double("range_tht_max");
		opt.range_rec_aspect[0] = as_double("range_rec_aspect_min");
		opt.range_rec_aspect[1] = as_double("range_rec_aspect_max");
		opt.range_rec_height[0] = as_double("range_rec_height_min");
		opt.range_rec_height[1] = as_double("range_rec_height_max");
		opt.flux_max = as_double("flux_max");
		*/


		helios.front().width = as_double("width");
		helios.front().height = as_double("height");
		helios.front().optical_error = as_double("optical_error");
		helios.front().active_fraction = as_double("active_fraction");
		helios.front().reflectance = as_double("reflectance");
		
		recs.front().absorptance = as_double("absorptance");
		recs.front().q_hl_perm2 = as_double("q_hl_perm2");
		
		layout.q_design = as_double("q_design");
		layout.land_max = as_double("land_max");
		layout.land_min = as_double("land_min");
		layout.h_tower = as_double("h_tower");
		
		cost.tower_fixed_cost = as_double("tower_fixed_cost");
		cost.tower_exp = as_double("tower_exp");
		cost.rec_ref_cost = as_double("rec_ref_cost");
		cost.rec_ref_area = as_double("rec_ref_area");
		cost.rec_cost_exp = as_double("rec_cost_exp");
		cost.site_spec_cost = as_double("site_spec_cost");
		cost.heliostat_spec_cost = as_double("heliostat_spec_cost");
		cost.plant_spec_cost = as_double("plant_spec_cost");
		cost.tes_spec_cost = as_double("tes_spec_cost");
		cost.land_spec_cost = as_double("land_spec_cost");
		cost.contingency_rate = as_double("contingency_rate");
		cost.sales_tax_rate = as_double("sales_tax_rate");
		cost.sales_tax_rate = as_double("sales_tax_frac");
		
	
		//set up the weather data for simulation
		const char *wffile = as_string("solar_resource_file" );
		if ( !wffile ) throw exec_error( "solarpilot", "no weather file specified" );
		weatherfile wf( wffile );
		if ( !wf.ok() || wf.type() == weatherfile::INVALID ) throw exec_error("solarpilot", "could not open weather file or invalid weather file format");

		
		amb.site_latitude = wf.lat;
		amb.site_longitude = wf.lon;
		amb.site_time_zone = wf.tz;

		vector<string> wfdata;
		wfdata.reserve( 8760 );
		char buf[1024];
		for( int i=0;i<8760;i++ )
		{
			if( !wf.read() )
				throw exec_error("solarpilot", "could not read data line " + util::to_string(i+1) + " of 8760 in weather file");

			mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
			wfdata.push_back( std::string(buf) );
		}

		sapi.SetSummaryCallback( solarpilot_callback, (void*)this);
		
		sapi.GenerateDesignPointSimulations( amb, V, wfdata );
	
		sapi.Setup(amb, cost, layout, helios, recs);

		sapi.CreateLayout();

		//	sapi.Optimize(opt, recs, layout);
	
		sp_optical_table opttab;
		sp_flux_table fluxtab;
		
		sapi.CalculateOpticalEfficiencyTable(opttab);

		if ( opttab.zeniths.size() > 0 && opttab.azimuths.size() > 0
			&& opttab.eff_data.size() > 0 && opttab.eff_data[0].size() > 0 )
		{
			ssc_number_t *zeniths = allocate( "opteff_zeniths", opttab.zeniths.size() );
			for( size_t i=0;i<opttab.zeniths.size();i++ )
				zeniths[i] = (float)opttab.zeniths[i];

			ssc_number_t *azimuths = allocate( "opteff_azimuths", opttab.azimuths.size() );
			for( size_t i=0;i<opttab.azimuths.size();i++ )
				azimuths[i] = (float)opttab.azimuths[i];

			size_t nrows = opttab.eff_data.size();
			size_t ncols = opttab.eff_data[0].size();
			ssc_number_t *opteff = allocate( "opteff_table", nrows, ncols );
			for( size_t i=0;i<nrows;i++ )
				for( size_t j=0;j<ncols;j++ )
					opteff[ i*ncols + j ] = (ssc_number_t)opttab.eff_data[i][j];
		}
		else
			throw exec_error("solarpilot", "failed to calculate a correct optical efficiency table");

		
		sapi.CalculateFluxMaps(fluxtab);

	}
};

static bool solarpilot_callback( simulation_info *siminfo, void *data )
{
	cm_solarpilot *cm = static_cast<cm_solarpilot*>( data );
	if ( !cm ) return false;
	float simprogress = (float)siminfo->getCurrentSimulation()/(float)(max(siminfo->getTotalSimulationCount(),1));
	return cm->update( *siminfo->getSimulationNotices(),
		simprogress*100.0f );

}

DEFINE_MODULE_ENTRY( solarpilot, "SolarPILOT - CSP tower solar field layout tool.", 0 )
