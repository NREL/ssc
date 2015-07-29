#include "csp_common.h"
#include "core.h"
#include "lib_weatherfile.h"
#include "lib_util.h"
#include <sstream>

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
    cmap[0] = Heliostat::CANT_METHOD::NONE;
    cmap[1] = Heliostat::CANT_METHOD::AT_SLANT;
    cmap[2] = cmap[3] = cmap[4] = Heliostat::CANT_METHOD::OFF_AXIS_DAYHOUR;
	
	int cant_type = m_cmod->as_integer("cant_type");

	helios.front().cant_type = cmap[ cant_type ];       //Convert to the Heliostat::CANT_METHOD list
    switch (cant_type)
    {
    case sp_heliostat::CANT_TYPE::NONE:
    case sp_heliostat::CANT_TYPE::ON_AXIS:
        //do nothing
        break;
    case sp_heliostat::CANT_TYPE::EQUINOX:
        helios.front().cant_settings.point_day = 81;  //spring equinox
		helios.front().cant_settings.point_hour = 12.;
        break;
    case sp_heliostat::CANT_TYPE::SOLSTICE_SUMMER:
        helios.front().cant_settings.point_day = 172;  //Summer solstice
		helios.front().cant_settings.point_hour = 12.;
        break;
    case sp_heliostat::CANT_TYPE::SOLSTICE_WINTER:
        helios.front().cant_settings.point_day = 355;  //Winter solstice
		helios.front().cant_settings.point_hour = 12.;
        break;
    default:
    {
        stringstream msg;
        msg << "Invalid Cant Type specified in AutoPILOT API. Method must be one of: \n" <<
               "NONE(0), ON_AXIS(1), EQUINOX(2), SOLSTICE_SUMMER(3), SOLSTICE_WINTER(4).\n" <<
               "Method specified is: " << cant_type << ".";
        throw spexception(msg.str());
    }
        break;
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
	weatherfile wFile( wffile );
	if ( !wFile.ok() || wFile.type() == weatherfile::INVALID ) throw compute_module::exec_error("solarpilot", wFile.message());

	weather_header hdr;
	wFile.header( &hdr );
		
	amb.site_latitude = hdr.lat;
	amb.site_longitude = hdr.lon;
	amb.site_time_zone = hdr.tz;
	amb.atten_model = sp_ambient::ATTEN_MODEL::USER_DEFINED;
	amb.user_atten_coefs.clear();
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_0") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_1") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_2") );
	amb.user_atten_coefs.push_back( m_cmod->as_double("c_atm_3") );

	weather_record wf;

	vector<string> wfdata;
	wfdata.reserve( 8760 );
	char buf[1024];
	for( int i=0;i<8760;i++ )
	{
		if( !wFile.read( &wf ) )
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
	if( m_cmod->as_boolean("calc_fluxmaps") ){      // <<--- was set "false" for some reason

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

bool solarpilot_invoke::postsim_calcs(compute_module *cm)
{
    /* 
    Update calculated values and cost model number to be used in subsequent simulation and analysis.

    The variable values used in this are consistent with the solarpilot compute module. These same variables are used in all 
    tower modules that use solarpilot API.

    */


    //receiver calculations
    double H_rec = recs.front().height;
    double rec_aspect = recs.front().aspect;
    double THT = layout.h_tower;
    //update heliostat position table
    int nr = (int)layout.heliostat_positions.size();
    ssc_number_t *ssc_hl = cm->allocate( "helio_positions", nr, 2 );
    for(int i=0; i<nr; i++){
        ssc_hl[i*2] = (ssc_number_t)layout.heliostat_positions.at(i).location.x;
        ssc_hl[i*2+1] = (ssc_number_t)layout.heliostat_positions.at(i).location.y;
    }

    double A_sf = cm->as_double("helio_height") * cm->as_double("helio_width") * cm->as_double("dens_mirror") * (double)nr;

    //update piping length for parasitic calculation
    double piping_length = THT * cm->as_double("csp.pt.par.piping_length_mult") + cm->as_double("csp.pt.par.piping_length_const");
            
    //update assignments for cost model
	cm->assign("H_rec", var_data((ssc_number_t)H_rec));
    cm->assign("rec_height", var_data((ssc_number_t)H_rec));
	cm->assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
    cm->assign("D_rec", var_data((ssc_number_t)(H_rec/rec_aspect)));
	cm->assign("THT", var_data((ssc_number_t)THT));
    cm->assign("h_tower", var_data((ssc_number_t)THT));
	cm->assign("A_sf", var_data((ssc_number_t)A_sf));
    cm->assign("Piping_length", var_data((ssc_number_t)piping_length) );

    //Update the total installed cost
    double total_direct_cost = 0.;
    double A_rec;
    switch (recs.front().type)
    {
    case sp_receiver::TYPE::CYLINDRICAL:
    {
        double h = recs.front().height;
        double d = h/recs.front().aspect;
        A_rec =  h*d*3.1415926;
        break;
    }
    case sp_receiver::TYPE::CAVITY:
    case sp_receiver::TYPE::FLAT:
        double h = recs.front().height;
        double w = h/recs.front().aspect;
        A_rec = h*w;
        break;
    }
    double receiver = cm->as_double("rec_ref_cost")*pow(A_rec/cm->as_double("rec_ref_area"), cm->as_double("rec_cost_exp"));     //receiver cost

    //storage cost
    double storage = cm->as_double("q_pb_design")*cm->as_double("tshours")*cm->as_double("tes_spec_cost")*1000.;

    //power block + BOP
    double P_ref = cm->as_double("P_ref") * 1000.;  //kWe
    double power_block = P_ref * (cm->as_double("plant_spec_cost") + cm->as_double("bop_spec_cost") ); //$/kWe --> $

    //site improvements
    double site_improvements = A_sf * cm->as_double("site_spec_cost");
            
    //heliostats
    double heliostats = A_sf * cm->as_double("heliostat_spec_cost");
            
    //fixed cost
    double cost_fixed = cm->as_double("cost_sf_fixed");

    //fossil
    double fossil = P_ref * cm->as_double("fossil_spec_cost");

    //tower cost
    double tower = cm->as_double("tower_fixed_cost") * exp( cm->as_double("tower_exp") * (THT + 0.5*(-H_rec + cm->as_double("helio_height")) ) );

    //---- total direct cost -----
    total_direct_cost = (1. + cm->as_double("contingency_rate")/100.) * (
        site_improvements + heliostats + power_block + 
        cost_fixed + storage + fossil + tower + receiver);
    //-----

    //land area
    double land_area = layout.land_area * cm->as_double("csp.pt.sf.land_overhead_factor") + cm->as_double("csp.pt.sf.fixed_land_area");

    //EPC
    double cost_epc = 
        cm->as_double("csp.pt.cost.epc.per_acre") * land_area
        + cm->as_double("csp.pt.cost.epc.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.epc.per_watt") 
        + cm->as_double("csp.pt.cost.epc.fixed");

    //PLM
    double cost_plm = 
        cm->as_double("csp.pt.cost.plm.per_acre") * land_area
        + cm->as_double("csp.pt.cost.plm.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.plm.per_watt") 
        + cm->as_double("csp.pt.cost.plm.fixed");

    //sales tax
    //return ${csp.pt.cost.sales_tax.value}/100*${total_direct_cost}*${csp.pt.cost.sales_tax.percent}/100; };
    double cost_sales_tax = cm->as_double("sales_tax_rate")/100. * total_direct_cost * cm->as_double("sales_tax_frac")/100.;

    //----- indirect cost
    double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;
            
    //----- total installed cost!
    double total_installed_cost = total_direct_cost + total_indirect_cost;
    cm->assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost ));

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