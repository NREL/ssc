#include "csp_common.h"
#include "core.h"
#include "lib_weatherfile.h"
#include "lib_util.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

static bool solarpilot_callback( simulation_info *siminfo, void *data );
static bool optimize_callback( simulation_info *siminfo, void *data );

solarpilot_invoke::solarpilot_invoke(compute_module *cm)
{
    m_cmod = cm;
    //anything else?
    m_sapi = 0;

}

solarpilot_invoke::~solarpilot_invoke()
{
    if(m_sapi != 0)
        delete m_sapi;
}

AutoPilot_S *solarpilot_invoke::GetSAPI()
{
    return m_sapi;
}

bool solarpilot_invoke::run()
{
    /* 
    
    */
    if(m_sapi != 0)
        delete m_sapi;

    m_sapi = new AutoPilot_S();

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
		
	bool isopt = m_cmod->as_boolean( "is_optimize" );
    if(isopt){
		opt.flux_max = m_cmod->as_double("flux_max");
        opt.max_step = m_cmod->as_double("opt_init_step");
        opt.max_iter = m_cmod->as_integer("opt_max_iter");
        opt.converge_tol = m_cmod->as_double("opt_conv_tol");
        opt.method = m_cmod->as_integer("opt_algorithm");
        opt.is_optimize_bound = false;
    }

	helios.front().width = m_cmod->as_double("helio_width");
	helios.front().height = m_cmod->as_double("helio_height");
	helios.front().optical_error = m_cmod->as_double("helio_optical_error"); 
	helios.front().active_fraction = m_cmod->as_double("helio_active_fraction") * m_cmod->as_double("dens_mirror");
	helios.front().reflectance = m_cmod->as_double("helio_reflectance");
	helios.front().npanels_h = m_cmod->as_integer("n_facet_y");
	helios.front().npanels_w = m_cmod->as_integer("n_facet_x");
	int cmap[5];
	cmap[0] = sp_heliostat::CANT_TYPE::FLAT;
	cmap[1] = sp_heliostat::CANT_TYPE::AT_SLANT;
	cmap[2] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
	cmap[3] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
	cmap[4] = sp_heliostat::CANT_TYPE::AT_DAY_HOUR;
	int cant_type = m_cmod->as_integer("cant_type");
	helios.front().cant_type = cmap[ cant_type ];
	if( cant_type == 2 ){
		helios.front().cant_settings.point_day = 81;  //spring equinox
		helios.front().cant_settings.point_hour = 12.;
	}
	else if( cant_type == 3 ){
		helios.front().cant_settings.point_day = 172;  //Summer solstice
		helios.front().cant_settings.point_hour = 12.;
	}
	else if( cant_type == 4){
		helios.front().cant_settings.point_day = 355;  //Winter solstice
		helios.front().cant_settings.point_hour = 12.;
	}

	int fmap[2];
	fmap[0] = sp_heliostat::FOCUS_TYPE::FLAT;
	fmap[1] = sp_heliostat::FOCUS_TYPE::AT_SLANT;
	helios.front().focus_type = fmap[ m_cmod->as_integer("focus_type") ];


	recs.front().absorptance = m_cmod->as_double("rec_absorptance");
	recs.front().height = m_cmod->as_double("rec_height");
	recs.front().aspect = m_cmod->as_double("rec_aspect");
	recs.front().q_hl_perm2 = m_cmod->as_double("rec_hl_perm2");
		
	layout.q_design = m_cmod->as_double("q_design");
	layout.dni_design = m_cmod->as_double("dni_des");
	layout.land_max = m_cmod->as_double("land_max");
	layout.land_min = m_cmod->as_double("land_min");
	layout.h_tower = m_cmod->as_double("h_tower");
		
	cost.tower_fixed_cost = m_cmod->as_double("tower_fixed_cost");
	cost.tower_exp = m_cmod->as_double("tower_exp");
	cost.rec_ref_cost = m_cmod->as_double("rec_ref_cost");
	cost.rec_ref_area = m_cmod->as_double("rec_ref_area");
	cost.rec_cost_exp = m_cmod->as_double("rec_cost_exp");
	cost.site_spec_cost = m_cmod->as_double("site_spec_cost");
	cost.heliostat_spec_cost = m_cmod->as_double("heliostat_spec_cost");
	cost.plant_spec_cost = m_cmod->as_double("plant_spec_cost") + m_cmod->as_double("bop_spec_cost");
	cost.tes_spec_cost = m_cmod->as_double("tes_spec_cost");
	cost.land_spec_cost = m_cmod->as_double("land_spec_cost");
	cost.contingency_rate = m_cmod->as_double("contingency_rate");
	cost.sales_tax_rate = m_cmod->as_double("sales_tax_rate");
	cost.sales_tax_rate = m_cmod->as_double("sales_tax_frac");
	cost.cost_fixed = m_cmod->as_double("cost_sf_fixed");
	
	//set up the weather data for simulation
	const char *wffile = m_cmod->as_string("solar_resource_file" );
	if ( !wffile ) throw compute_module::exec_error( "solarpilot", "no weather file specified" );
	weatherfile wf( wffile );
	if ( !wf.ok() || wf.type() == weatherfile::INVALID ) throw compute_module::exec_error("solarpilot", wf.error_message());

		
	amb.site_latitude = wf.lat;
	amb.site_longitude = wf.lon;
	amb.site_time_zone = wf.tz;
	amb.atten_model = sp_ambient::ATTEN_MODEL::USER_DEFINED;
	amb.user_atten_coefs.clear();
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_0") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_1") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_2") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_3") );

	vector<string> wfdata;
	wfdata.reserve( 8760 );
	char buf[1024];
	for( int i=0;i<8760;i++ )
	{
		if( !wf.read() )
			throw compute_module::exec_error("solarpilot", "could not read data line " + util::to_string(i+1) + " of 8760 in weather file");

		mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
		wfdata.push_back( std::string(buf) );
	}

	m_sapi->SetDetailCallback( solarpilot_callback, m_cmod);
	m_sapi->SetSummaryCallbackStatus(false);

	m_sapi->GenerateDesignPointSimulations( amb, V, wfdata );
	
    if(isopt){
        m_cmod->log("Optimizing...", SSC_WARNING, 0.);
        m_sapi->SetSummaryCallback( optimize_callback, m_cmod);
		m_sapi->Setup(amb, cost, layout, helios, recs, true);
            
        if(! m_sapi->Optimize(opt, recs, layout) )
            return false;

        m_sapi->SetSummaryCallbackStatus(false);
        m_sapi->PreSimCallbackUpdate();
            
    }
    else{
		m_sapi->Setup(amb, cost, layout, helios, recs);
    }
    if(! m_sapi->CreateLayout() )
        return false;

    //check if flux map calculations are desired
	if( false ){ //m_cmod->as_boolean("calc_fluxmaps") ){

		m_sapi->SetDetailCallbackStatus(false);
		m_sapi->SetSummaryCallbackStatus(true);
		m_sapi->SetSummaryCallback( solarpilot_callback, m_cmod );
		
	
		//sp_optical_table opttab;
		fluxtab.is_user_spacing = true;
		fluxtab.n_flux_days = m_cmod->as_integer("n_flux_days");
		fluxtab.delta_flux_hrs = m_cmod->as_integer("delta_flux_hrs");
		
		int nflux_x = 12, nflux_y = 1;
		if(! m_sapi->CalculateFluxMaps(fluxtab, nflux_x, nflux_y, true) )
            return false;  //simulation failed or was cancelled.
            
		//collect the optical efficiency data and sun positions
		if ( fluxtab.zeniths.size() == 0 || fluxtab.azimuths.size() == 0
			|| fluxtab.efficiency.size() == 0 )
			throw compute_module::exec_error("solarpilot", "failed to calculate a correct optical efficiency table");
		
		//collect the flux map data
		block_t<double> *flux_data = &fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
		if( flux_data->ncols() == 0 || flux_data->nlayers() == 0 )
			throw compute_module::exec_error("solarpilot", "failed to calculate a correct flux map table");
	}
        
    return true;
}


static bool solarpilot_callback( simulation_info *siminfo, void *data )
{
	compute_module *cm = static_cast<compute_module*>( data );
	if ( !cm ) return false;
	float simprogress = (float)siminfo->getCurrentSimulation()/(float)(max(siminfo->getTotalSimulationCount(),1));

	return cm->update( *siminfo->getSimulationNotices(),
		simprogress*100.0f );

}

static bool optimize_callback( simulation_info *siminfo, void *data )
{
    compute_module *cm = static_cast<compute_module*>( data );
    if(! cm) return false;
    
    std::string notices = *siminfo->getSimulationNotices();
    cm->log( notices, SSC_WARNING, 0. );
    
    return true;
}