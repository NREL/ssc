#include "AutoPilot_API.h"
#include "LayoutSimulateThread.h"
#include "IOUtil.h"
#include "SolarField.h"
#include "definitions.h"
#include <iomanip> 

#ifdef SP_USE_THREADS
#include <thread>
#endif

#include <sstream>
#include <string>
#include <algorithm>

#include <nlopt.hpp>

using namespace std;


template<typename T> static std::string my_to_string( T value )
{
	std::ostringstream os;
	os << value;
	return os.str();
}

class response_surface_data
{
public:
	int N_vars;
	int Order;
	vector<vector<int> > generators;
	vector<double> Y;
	vector<vector<double> > X;
	vector<double> cur_pos;
	vector<double> Beta;
	int ncalls;
	//double big_M;
	double max_step_size;

	response_surface_data(){
		ncalls = 0;
	}

	int CalcNumberBetas(){
		//calculate the number of BETA terms
		int nbeta = 0;
		for(int i=N_vars+1; i>0; i--)
			nbeta += i;
		return nbeta;
	};

	double EvaluateBiLinearResponse(vector<double> &xpt)
	{
		/* 
		Evaluate a function of the form
		F(x) = beta0 + beta1 * x1 + beta2 * x2 .. + betaN * x1 * x2 + ... + betaP x1^2 ...

		Only cross-terms up to bi-linear form (e.g. x1 * x2) will be included. Quadratic terms
		are also included.
		*/

		
		//is Beta ok?
		/*if(Beta.size() != nbeta)
			Beta.resize(nbeta, 1.);*/

		double yret = 0.;
		int ib=0;
		for(int i=0; i<N_vars+1; i++){
			double xi = i==0 ? 1. : xpt.at(i-1);
			for(int j=i; j<N_vars+1; j++){
				double xj = j==0 ? 1. : xpt.at(j-1);
				yret += xi * xj * Beta.at(ib++);
			}
		}

		return yret;
	};
	
	void AddGenerator(vector<vector<int> > &design, int a, int b = 0, int c = 0, int d = 0, int e = 0)
	{
		vector<int> newgen;
		if(a>0) newgen.push_back(a);
		if(b>0) newgen.push_back(b);
		if(c>0) newgen.push_back(c);
		if(d>0) newgen.push_back(d);
		if(e>0) newgen.push_back(e);
	}

	void GenerateSurfaceEvalPoints( vector<double> &point, vector<vector<double> > &sim_points, double tolerance)
	{
		/* 
		Take a current point (vector of doubles) that is normalized, and calculate a design of experiments run that 
		would produce a linear estimate of the local surface. Parameters are varied up/down by the tolerance. The
		runs required are returned in the "sim_points" array.

		2-level fractional factorial design information is from Wu & Hamada (2000) "Experiments" Appendix 4a Tab 4A.3
		*/
	
		int nvars = (int)point.size();

		sim_points.clear();

		//allow up to a 32-run table
		int nruns = (int)pow(2., min(nvars,5));

		//values that determine when each variable toggles value
		vector<int> divisors;
		for(int i=0; i<nvars; i++)
			divisors.push_back( (int)pow(2., i) );
		//create a design with either +1 or -1 as high/low value
		vector<vector<int> > design;
		design.push_back( vector<int>(nvars, 1) );
	
		//Create the base table
		for(int i=1; i<nruns; i++){
			vector<int> newline(nvars, 0);
			for(int j=0; j<min(nvars,5); j++)
				newline.at(j) =  design.at(i-1).at(j) * (int)(fmod((double)i, (double)divisors.at(j) ) == 0 ? -1 : 1 ) ;
			design.push_back(newline);
		}
	
		//if there are more than 5 variables, create a fractional factorial design with 32 runs
		generators.clear();
		switch(nvars)
		{
		default:
			if(nvars > 16)
				throw spexception("Optimization is supported for up to 16 independent variables. "
				"Please reduce the number of optimization variables and try again.");
			break;

		case 6:
			AddGenerator( generators, 1,2,3,4,5 );
			break;
		case 7:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4,5 );
			break;
		case 8:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,3,4,5 );
			break;
		case 9:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,4,5 );
			break;
		case 10:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,4,5 );
			AddGenerator( generators, 2,3,4,5 );
			break;
		case 11:
		case 12:
		case 13:
		case 14:
		case 15:
		case 16:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,3,4 );
			AddGenerator( generators, 2,3,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,5 );
			if(nvars == 11) break;
			AddGenerator( generators, 2,3,5 );
			if(nvars == 12) break;
			AddGenerator( generators, 1,4,5 );
			if(nvars == 13) break;
			AddGenerator( generators, 2,4,5 );
			if(nvars == 14) break;
			AddGenerator( generators, 3,4,5 );
			if(nvars == 15) break;
			AddGenerator( generators, 1,2,3,4,5 );
			break;

		}

		//Use the design generators to fill in additional variable columns, if needed
		for(int j=5; j<nvars; j++){
			for(int i=0; i<nruns; i++){

				int val = 1;
				for(int k=0; k<(int)generators.at(j-5).size(); k++)
					val *= design.at(i).at( generators.at(j-5).at(k) - 1 );
				design.at(i).at(j) = val;
			
			}
		}

		//Add central composite runs
		for(int i=0; i<nvars; i++){
			vector<int> row(nvars, 0);
			row.at(i) = 1;
			design.push_back(row);
			row.at(i) = -1;
			design.push_back(row);
		}
		nruns = (int)design.size();

		//create the sim_points array with actual non-dimensionalized simulation values rather than integer surrogates
		for(int i=0; i<nruns; i++){
			sim_points.push_back( vector<double>(nvars) );
			for(int j=0; j<nvars; j++){
				sim_points.at(i).at(j) = point.at(j) * (1. + tolerance * (double)design.at(i).at(j) );
			}
		}
		
	}

};

struct AutoOptHelper
{
    int m_iter;
    AutoPilot *m_autopilot;
    vector<vector<double> > m_all_points;
    vector<double> m_normalizers;
    vector<double> m_objective;
    vector<double> m_flux;
    vector<double*> m_opt_vars;
    vector<string> m_opt_names;
    sp_receivers *m_recs;
    sp_optimize *m_opt;
    sp_layout *m_layout;
    nlopt::opt *m_opt_obj;

    void SetObjects( void *autopilot, sp_receivers *recs, sp_optimize *opt, sp_layout *layout, nlopt::opt *optobj ){
        m_autopilot = static_cast<AutoPilot*>( autopilot );
        m_recs = recs;
        m_opt = opt;
        m_layout = layout;
        m_opt_obj = optobj;
    };

    void Initialize()
    {
        m_iter = 0;
        m_autopilot = 0;
        m_recs = 0;
        m_opt = 0;
        m_layout = 0;
        m_opt_obj = 0;
        m_all_points.clear();
        m_normalizers.clear();
        m_objective.clear();
        m_flux.clear();
        m_opt_vars.clear();
        m_opt_names.clear();
    };

    double Simulate(const double *x, int n)
    {
        /* 
        Run a simulation and update points as needed. Report outcome from each step.
        */
        if(m_autopilot->IsSimulationCancelled() )
        {
            m_opt_obj->force_stop();
            return 0.;
        }

        m_iter += 1;

        vector<double> current;

        //update the objective variables
        for(int i=0; i<(int)m_opt_vars.size(); i++)
        {
            current.push_back( x[i] );
            *m_opt_vars.at(i) = current.at(i) * m_normalizers.at(i);
        }

        m_all_points.push_back( current );

        double obj, flux;
        
        //Evaluate the objective function value
        if(! 
        m_autopilot->EvaluateDesign( *m_opt, *m_recs, *m_layout, obj, flux) 
            ){
            string errmsg = "Optimization failed at iteration " + my_to_string(m_iter) + ". Terminating simulation.";   
            throw spexception(errmsg.c_str());
        }
        //Update variables as needed
        m_autopilot->PostEvaluationUpdate(m_iter, current, m_normalizers, obj, flux);

        m_objective.push_back(obj);
        m_flux.push_back(flux);
                
        return obj;
    };
};

double optimize_leastsq_eval(unsigned n, const double *x, double *grad, void *data)
{
	/* 
	Evaluate the residual sum of squares
	*/
	
	response_surface_data *D = static_cast<response_surface_data*>( data );
	D->ncalls ++;
	//We are solving for the Beta coefficients (contained in X)
	if(D->Beta.size() != n){
		D->Beta.resize(n,1.);
	}

	for(unsigned i=0; i<n; i++)
		D->Beta.at(i) = x[i];

	double ssres=0.;

	for(int i=0; i<(int)D->X.size(); i++){
		double y = D->EvaluateBiLinearResponse(D->X.at(i));
		double ssrv = (y - D->Y.at(i));
		ssres += ssrv * ssrv;	//residual sum of squares
	}

	return ssres; 

};

double optimize_stdesc_eval(unsigned n, const double *x, double *grad, void *data)
{
	/* 
	Minimize the response surface value subject to a maximum step size.
	*/

	response_surface_data *D = static_cast<response_surface_data*>(data);
	D->ncalls ++;
	vector<double> xpt;
	//double ssize = 0.;
	for(unsigned i=0; i<n; i++){
		xpt.push_back(x[i]);
		//double xistep = x[i] - D->cur_pos.at(i);
		//D->cur_pos.at(i) = x[i];

		//ssize += xistep*xistep;
	}
	//ssize = sqrt(ssize);

	//return D->EvaluateBiLinearResponse( xpt ) + abs(ssize - D->max_step_size)*D->big_M;
	return D->EvaluateBiLinearResponse( xpt );

};

double optimize_maxstep_eval(unsigned n, const double *x, double *grad, void *data)
{
	
	response_surface_data *D = static_cast<response_surface_data*>(data);

	vector<double> xpt;
	double ssize = 0.;
	for(unsigned i=0; i<n; i++){
		xpt.push_back(x[i]);
		double xistep = x[i] - D->cur_pos.at(i);
		//D->cur_pos.at(i) = x[i];

		ssize += xistep*xistep;
	}
	ssize = sqrt(ssize);
	return ssize - D->max_step_size;

}

double optimize_auto_eval(unsigned n, const double *x, double *grad, void *data)
{
    AutoOptHelper *D = static_cast<AutoOptHelper*>( data );
    //Only calls to methods available in AutoPilot base class are allowed!

    return D->Simulate(x, n);
};



void TestQuadSurface(vector<double*> &vptrs, double &opt, double &flux)
{
	//something random but regressable.
	opt = 0.;
	for(int i=0; i<(int)vptrs.size(); i++)
		//opt += sin(*vptrs.at(i) );
		opt += i * (*vptrs.at(i) ) + (*vptrs.at(i))*(*vptrs.at(max(i-1,0))) + (*vptrs.at(i))*(*vptrs.at(i))*i;
	flux = 2 * opt;

};

AutoPilot::AutoPilot()
{
	_has_summary_callback = false;
	_has_detail_callback = false;
	_is_solarfield_external = false;
	_SF = 0;
	_summary_callback = 0;
	_detail_callback = 0;
	_summary_callback_data = 0;
	_detail_callback_data = 0;
	_summary_siminfo = 0;
	_detail_siminfo = 0;
}

AutoPilot::~AutoPilot()
{
	if( _SF != 0 ){
		//quietly try to delete the solar field
		try{
			delete _SF;
		}
		catch(...){}
	}

	if( _summary_siminfo != 0)
	{
		//quietly try to delete the simulation info object
		try{
			delete _summary_siminfo;
		}
		catch(...){}
	}
	return;
}

bool AutoPilot::CreateLayout(bool do_post_process)
{
	
	//override in inherited class
	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
	return false;
};

bool AutoPilot::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	//override in inherited class
	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
	return false;
};

bool AutoPilot::CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	//override in inherited class
	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
	return false;
};

void AutoPilot::SetSummaryCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata)
{
	_has_summary_callback = true;
	_summary_callback = callback;
	_summary_callback_data = cdata;
}

void AutoPilot::SetDetailCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata)
{
	_has_detail_callback = true;
	_detail_callback = callback;
	_detail_callback_data = cdata;
}

void AutoPilot::SetSummaryCallbackStatus(bool is_enabled)
{
	_has_summary_callback = is_enabled;
}

void AutoPilot::SetDetailCallbackStatus(bool is_enabled)
{
	_has_detail_callback = is_enabled;
	if(_SF != 0)
		_SF->getSimInfoObject()->isEnabled(is_enabled);
}

void AutoPilot::SetExternalSFObject( SolarField *SF )
{
	_SF = SF;
	_is_solarfield_external = true;
}

bool AutoPilot::Setup(sp_ambient &ambient, sp_cost &cost, sp_layout &layout, sp_heliostats &helios, sp_receivers &recs, bool for_optimize){

	/* 
	Using the information provided in the data structures, construct a new SolarField object.
	Structures are declared in "API_structures.h"

	Any messages can be provided back through the optional 'messages' vector, if provided.

	Returns:
	True	-	no errors setting up the field
	False	-	errors setting up the field
	*/
	_cancel_simulation = false;	

	//start by initializing the variables structure
	ioutil::parseDefinitionArray(_variables, "custom_rec");

	//Save pointers for later reference
	_layout = &layout;
    _cost = &cost;

	//-----------------------------------------------------------------
	//	Alter settings according to what's provided in the structures
	//-----------------------------------------------------------------
	
	//ambient
	update_ambient(_variables, ambient);

	//cost
	update_cost(_variables, cost);

	//layout
	update_layout(_variables, layout);

	//heliostats
	update_heliostats(_variables, helios);

	//receivers
	update_receivers(_variables, recs);
	
	//update any calculated values
	interop::UpdateCalculatedMapValues(_variables);

	//Dynamically allocate the solar field object, if needed
	if(! _is_solarfield_external ){
		_SF = new SolarField();
	}

	//---Set a couple of parameters here that should be consistent for simple API use
	
	//make sure the aiming strategy is correct
	if(recs.front().type == sp_receiver::TYPE::CYLINDRICAL && !for_optimize)
		_variables["fluxsim"][0]["aim_method"].set( FluxSimData::AIM_STRATEGY::SIMPLE );
	else
    {
		_variables["fluxsim"][0]["aim_method"].set( FluxSimData::AIM_STRATEGY::IMAGE_SIZE );
        _variables["fluxsim"][0]["sigma_limit"].set(2.5);
    }

	//set the receiver flux surfaces to the correct resolution to balance run time with accuracy
	if( _variables["receiver"][0]["rec_type"].value_int() == 0 ){
		//external receiver
		_variables["fluxsim"][0]["x_res"].set(12);
		_variables["fluxsim"][0]["y_res"].set(20);
	
	}
	else{
		//flat plate receiver
		_variables["fluxsim"][0]["x_res"].set(15);
		_variables["fluxsim"][0]["y_res"].set(15);
	}
	
	//Create the solar field object
	_SF->Create(_variables);

	//if a layout is provided in the sp_layout structure, go ahead and create the geometry here.
	if(layout.heliostat_positions.size() > 0){
		WeatherData empty;
		_SF->PrepareFieldLayout(*_SF, empty, true);	//Run the layout method in refresh_only mode
		_SF->calcHeliostatShadows();
        double area = _SF->getLandObject()->getLandArea() /4046.85642;  //m2->acre
		_variables["land"][0]["bound_area"].set( area );
        layout.land_area = area;
	}

	
	PreSimCallbackUpdate();
	_setup_ok = true;
	
	return true;
}

void AutoPilot::LoadAllDefaultValues(sp_ambient &ambient, sp_cost &cost, sp_layout &layout, 
                                     sp_heliostats &helios, sp_receivers &recs, sp_optimize &opt, var_set *variables)
{
    bool var_in = variables == 0;

    var_set *var_use;

    if(var_in){
        var_use = variables;
    }
    else{
        var_use = new var_set();
    }

    ioutil::parseDefinitionArray(*var_use);

    vector<string> wfdummy; //not used
    SetupExpert(*var_use, ambient, cost, layout, helios, recs, opt, wfdummy, true);


    if(! var_in) delete var_use;


}

bool AutoPilot::SetupExpert(var_set &vset, sp_ambient &ambient, sp_cost &cost, sp_layout &layout, sp_heliostats &helios, 
                            sp_receivers &recs, sp_optimize &opt, vector<string> &weather_data, bool defaults_only)
{
	_cancel_simulation = false;

	//load the values from the var_set into the input structures
	ambient.LoadDefaults(vset);
	cost.LoadDefaults(vset);
	layout.LoadDefaults(vset);
	helios.resize(1);
	for(int i=0; i<(int)helios.size(); i++)
		helios.at(i).LoadDefaults(vset);
	recs.resize(1);
	for(int i=0; i<(int)recs.size(); i++)
		recs.at(i).LoadDefaults(vset);
	opt.LoadDefaults(vset);

    if(defaults_only) return true;
	
    //set up the weather data for simulation
	GenerateDesignPointSimulations(ambient, vset, weather_data);


	//Dynamically allocate the solar field object, if needed
	if(! _is_solarfield_external ){
		_SF = new SolarField();
	}
	_variables = vset;	//copy
	
	//update any calculated values
	interop::UpdateCalculatedMapValues(_variables);

	//Create the solar field object
	_SF->Create(_variables);
	
	//if a layout is provided in the sp_layout structure, go ahead and create the geometry here.
	if(layout.heliostat_positions.size() > 0){
		WeatherData empty;
		_SF->PrepareFieldLayout(*_SF, empty, true);	//Run the layout method in refresh_only mode
		_SF->calcHeliostatShadows();
		_variables["land"][0]["bound_area"].set( _SF->getLandObject()->getLandArea() );
	}

	//pass the callback along to the solar field, if applicable
	if(_has_detail_callback){
		_detail_siminfo = _SF->getSimInfoObject();
		_detail_siminfo->setCallbackFunction(_detail_callback, _detail_callback_data);
		_detail_siminfo->isEnabled(true);
	}
	if(_has_summary_callback){
		_summary_siminfo = new simulation_info();
		_summary_siminfo->ResetValues();
		_summary_siminfo->setCallbackFunction(_summary_callback, _summary_callback_data);
	}
	_setup_ok = true;
	return true;
		
};

void AutoPilot::update_ambient(var_set &vset, sp_ambient &ambient){
	_variables["ambient"][0]["elevation"].set(ambient.site_elevation);
	_variables["ambient"][0]["longitude"].set(ambient.site_longitude);
	_variables["ambient"][0]["latitude"].set(ambient.site_latitude);
	_variables["ambient"][0]["time_zone"].set(ambient.site_time_zone);
	
	//attenuation model
	string avals;
	switch (ambient.atten_model)
	{
	case sp_ambient::ATTEN_MODEL::DELSOL_CLEAR_DAY:
		_variables["ambient"][0]["atm_model"].value = "0";
		_variables["ambient"][0]["atm_model"].cselect = 0;
		_variables["ambient"][0]["atm_coefs"].value = _variables["ambient"][0]["atm_coefs"].choices.at(0);
		break;
	case sp_ambient::ATTEN_MODEL::DELSOL_HAZY_DAY:
		_variables["ambient"][0]["atm_model"].value = "1";
		_variables["ambient"][0]["atm_model"].cselect = 1;
		_variables["ambient"][0]["atm_coefs"].value = _variables["ambient"][0]["atm_coefs"].choices.at(1);
		break;
	case sp_ambient::ATTEN_MODEL::USER_DEFINED:
		avals.clear();
		for(int i=0; i<(int)ambient.user_atten_coefs.size(); i++){
			avals.append( my_to_string(ambient.user_atten_coefs.at(i)) );
			if( i < ambient.user_atten_coefs.size()-1 ) avals.append(",");
		}
		
		_variables["ambient"][0]["atm_model"].value = "2";
		_variables["ambient"][0]["atm_model"].cselect = 2;
		_variables["ambient"][0]["atm_coefs"].value = avals;
		_variables["ambient"][0]["atm_coefs"].choices.at(2) = avals;
		break;
	default:
		if( _has_summary_callback ){
			_summary_siminfo->addSimulationNotice("Invalid atmospheric model number provided. Options are 0=Delsol clear day, 1=Delsol hazy day, 2=user coefs");
			return;
		}
		break;
	}
	
	//Sun shape
	_variables["ambient"][0]["sun_type"].set(ambient.sun_type);
	string *valptr;
	switch (ambient.sun_type)
	{
	case sp_ambient::SUN_TYPE::PILLBOX:
		if(ambient.sun_type_params.pillbox_width != std::numeric_limits<double>::quiet_NaN() )
			_variables["ambient"][0]["sun_rad_limit"].set(ambient.sun_type_params.pillbox_width);
		else
			_variables["ambient"][0]["sun_rad_limit"].value = "4.65";	//default to sun disc

		break;
	case sp_ambient::SUN_TYPE::GAUSSIAN:
		if(ambient.sun_type_params.gaussian_stdev != std::numeric_limits<double>::quiet_NaN() )
			_variables["ambient"][0]["sun_rad_limit"].set(ambient.sun_type_params.gaussian_stdev);
		else
			_variables["ambient"][0]["sun_rad_limit"].value = "2.73";	//default to SolTrace value

		break;
	case sp_ambient::SUN_TYPE::LIMB_DARKENED:
		break;
	case sp_ambient::SUN_TYPE::POINT:
		break;
	case sp_ambient::SUN_TYPE::BUIE:
		if(ambient.sun_type_params.circumsolar_ratio != std::numeric_limits<double>::quiet_NaN() )
			_variables["ambient"][0]["sun_csr"].set(ambient.sun_type_params.circumsolar_ratio);
		else
			_variables["ambient"][0]["sun_csr"].value = "0.1";

		break;
	case sp_ambient::SUN_TYPE::USER:
		//set up the table from the user-supplied values
		valptr = &_variables["ambient"][0]["user_sun"].value;
		valptr->clear();
		for(int i=0; i<(int)ambient.user_sun_data.size(); i++){
			valptr->append(my_to_string(ambient.user_sun_data.at(i).angle) );
			valptr->append(",");
			valptr->append(my_to_string(ambient.user_sun_data.at(i).intensity) );
			valptr->append(";");
		}
		break;
	default:
		if( _has_summary_callback )
			_summary_siminfo->addSimulationNotice("The specified sun shape model is invalid. Options are "
			"Pillbox sun=2;Gaussian sun=4;Limb-darkened sun=1;Point sun=0;Buie CSR=5;User sun=3;");
		break;
	}


	//Weather data
	
	valptr = &_variables["solarfield"][0]["sim_step_data"].value;
	valptr->clear();
	for(int i=0; i<(int)ambient.weather_data.size(); i++){
		valptr->append( ambient.weather_data.at(i).get_formatted_entry() );
	}

}

void AutoPilot::update_cost(var_set &vset, sp_cost &cost){
	vset["financial"][0]["tower_fixed_cost"].set(cost.tower_fixed_cost);
	vset["financial"][0]["tower_exp"].set(cost.tower_exp);
	vset["financial"][0]["rec_ref_cost"].set(cost.rec_ref_cost);
	vset["financial"][0]["rec_ref_area"].set(cost.rec_ref_area);
	vset["financial"][0]["rec_cost_exp"].set(cost.rec_cost_exp);
	vset["financial"][0]["site_spec_cost"].set(cost.site_spec_cost);
	vset["financial"][0]["heliostat_spec_cost"].set(cost.heliostat_spec_cost);
	vset["financial"][0]["wiring_user_spec"].set(cost.wiring_user_spec);
	vset["financial"][0]["plant_spec_cost"].set(cost.plant_spec_cost);
	vset["financial"][0]["tes_spec_cost"].set(cost.tes_spec_cost);
	vset["financial"][0]["land_spec_cost"].set(cost.land_spec_cost);
	vset["financial"][0]["contingency_rate"].set(cost.contingency_rate);
	vset["financial"][0]["sales_tax_rate"].set(cost.sales_tax_rate);
	vset["financial"][0]["sales_tax_frac"].set(cost.sales_tax_frac);
	vset["financial"][0]["sales_tax_cost"].set(cost.sales_tax_cost);
    vset["financial"][0]["fixed_cost"].set(cost.cost_fixed);
}

void AutoPilot::update_layout(var_set &vset, sp_layout &layout){
	//set the one-off values
	vset["solarfield"][0]["q_des"].set(layout.q_design);
	vset["solarfield"][0]["accept_max"].set(layout.span_cw);
	vset["solarfield"][0]["accept_min"].set(layout.span_ccw);
	vset["solarfield"][0]["tht"].set(layout.h_tower);
	vset["solarfield"][0]["dni_des"].set( layout.dni_design );

	//Handle land restrictions here
	switch (layout.land_bound_type)
	{
	case sp_layout::LAND_BOUND_TYPE::SCALED:
		vset["land"][0]["max_scaled_rad"].set(layout.land_max);
		vset["land"][0]["min_scaled_rad"].set(layout.land_min);
		vset["land"][0]["is_bounds_scaled"].set(true);
		vset["land"][0]["is_bounds_fixed"].set(false);
		vset["land"][0]["is_bouds_array"].set(false);
		break;
	case sp_layout::LAND_BOUND_TYPE::FIXED:
		vset["land"][0]["max_fixed_rad"].set(layout.land_max);
		vset["land"][0]["min_fixed_rad"].set(layout.land_min);
		vset["land"][0]["is_bounds_scaled"].set(false);
		vset["land"][0]["is_bounds_fixed"].set(true);
		vset["land"][0]["is_bouds_array"].set(false);
		break;
	case sp_layout::LAND_BOUND_TYPE::POLYGON:
	{
		vset["land"][0]["is_bounds_scaled"].set(false);
		vset["land"][0]["is_bounds_fixed"].set(true);
		vset["land"][0]["is_bouds_array"].set(false);

		//add the land table
		string incs, excs;
		//inclusions
		for( int i = 0; i<layout.landtable.inclusions.size(); i++){
			incs.append("[POLY]");
			for( int j = 0; j<layout.landtable.inclusions.at(i).size(); j++){
				incs.append("[P]" + 
					my_to_string(layout.landtable.inclusions.at(i).at(j).x) + "," +
					my_to_string(layout.landtable.inclusions.at(i).at(j).y));
			}
		}
		//exclusions
		for( int i = 0; i<layout.landtable.exclusions.size(); i++){
			incs.append("[POLY]");
			for( int j = 0; j<layout.landtable.exclusions.at(i).size(); j++){
				incs.append("[P]" + 
					my_to_string(layout.landtable.exclusions.at(i).at(j).x) + "," +
					my_to_string(layout.landtable.exclusions.at(i).at(j).y));
			}
		}

		vset["land"][0]["inclusions"].value = incs;
		vset["land"][0]["exclusions"].value = excs;
		break;
	}
	default:
		if( _has_summary_callback )
			_summary_siminfo->addSimulationNotice("The specified land bound type is invalid. Options are Scaled=0, Fixed=1, Polygon=2.");
		break;
	}

	//Handle specified layout positions, if needed
	if(layout.heliostat_positions.size() > 0){
		string layout_data;
		for(vector<sp_layout::h_position>::iterator hpos = layout.heliostat_positions.begin(); 
			hpos != layout.heliostat_positions.end(); hpos ++){
					layout_data.append( 
						my_to_string(hpos->template_number) + "," + 
						my_to_string(hpos->location.x) + "," +
						my_to_string(hpos->location.y) + "," +
						my_to_string(hpos->location.z) + ",");
			//if the user provides optics, use them here. Otherwise set values to null
			if(hpos->user_optics){
				layout_data.append(
					my_to_string(hpos->focal_length) + "," + 
					my_to_string(hpos->focal_length) + "," + 
					my_to_string(hpos->cant_vector.i) + "," +
					my_to_string(hpos->cant_vector.j) + "," +
					my_to_string(hpos->cant_vector.k) + "," +
					my_to_string(hpos->aimpoint.x) + "," + 
					my_to_string(hpos->aimpoint.y) + "," + 
					my_to_string(hpos->aimpoint.z) );
			}
			else{
				layout_data.append("NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL");
			}
			layout_data.append(";");	//end of line
		}
		vset["solarfield"][0]["layout_data"].value = layout_data;
	}

}

void AutoPilot::update_heliostats(var_set &vset, sp_heliostats &helios){
	int h=0;
	for( sp_heliostats::iterator helio = helios.begin(); helio != helios.end(); helio++){
		_variables["heliostat"][h]["width"].set(helio->width);
		_variables["heliostat"][h]["height"].set(helio->height);
		_variables["heliostat"][h]["n_cant_x"].set(helio->npanels_w);
		_variables["heliostat"][h]["n_cant_y"].set(helio->npanels_h);
        _variables["heliostat"][h]["is_faceted"].set( helio->npanels_h > 1 || helio->npanels_w > 1 );
		_variables["heliostat"][h]["err_elevation"].value = "0.";
		_variables["heliostat"][h]["err_azimuth"].value = "0.";
		_variables["heliostat"][h]["err_surface_x"].set(helio->optical_error);
		_variables["heliostat"][h]["err_surface_y"].set(helio->optical_error);
		_variables["heliostat"][h]["err_reflect_x"].value = "0.";
		_variables["heliostat"][h]["err_reflect_y"].value = "0.";
		_variables["heliostat"][h]["reflectivity"].set(helio->reflectance);
        _variables["heliostat"][h]["reflect_ratio"].set( helio->active_fraction );
		_variables["heliostat"][h]["soiling"].value = "1.";

		//Canting
		_variables["heliostat"][h]["cant_method"].set(helio->cant_type);
        
        switch (helio->cant_type)
        {
        case sp_heliostat::CANT_TYPE::NONE:
        case sp_heliostat::CANT_TYPE::ON_AXIS:
            //do nothing
            break;
        case sp_heliostat::CANT_TYPE::EQUINOX:
        case sp_heliostat::CANT_TYPE::SOLSTICE_SUMMER:
        case sp_heliostat::CANT_TYPE::SOLSTICE_WINTER:
            //set the day
            _variables["heliostat"][h]["cant_day"].set(helio->cant_settings.point_day);
			_variables["heliostat"][h]["cant_hour"].set(helio->cant_settings.point_hour);
            break;
        case -99:
            //this one isn't handled right now. This will be a placeholder
            _variables["heliostat"][h]["is_cant_vect_slant"].value = helio->cant_settings.scale_with_slant ? "TRUE" : "FALSE";
			_variables["heliostat"][h]["cant_vect_i"].set(helio->cant_settings.point_vector.i);
			_variables["heliostat"][h]["cant_vect_j"].set(helio->cant_settings.point_vector.j);
			_variables["heliostat"][h]["cant_vect_k"].set(helio->cant_settings.point_vector.k);
        default:
            break;
        }
/*
		switch (helio->focus_type)
		{
		case sp_heliostat::CANT_TYPE::FLAT:
		case sp_heliostat::CANT_TYPE::AT_SLANT:
			break;
		case sp_heliostat::CANT_TYPE::AT_DAY_HOUR:
			_variables["heliostat"][h]["cant_day"].set(helio->cant_settings.point_day);
			_variables["heliostat"][h]["cant_hour"].set(helio->cant_settings.point_hour);
			break;
		case sp_heliostat::CANT_TYPE::USER_VECTOR:
			_variables["heliostat"][h]["is_cant_vect_slant"].value = helio->cant_settings.scale_with_slant ? "TRUE" : "FALSE";
			_variables["heliostat"][h]["cant_vect_i"].set(helio->cant_settings.point_vector.i);
			_variables["heliostat"][h]["cant_vect_j"].set(helio->cant_settings.point_vector.j);
			_variables["heliostat"][h]["cant_vect_k"].set(helio->cant_settings.point_vector.k);
			break;
		default:
			break;
		}*/

		//Focusing
		_variables["heliostat"][h]["focus_method"].set(helio->focus_type);
		_variables["heliostat"][h]["is_focal_equal"].value = "TRUE";
		switch (helio->focus_type)
		{
		case sp_heliostat::FOCUS_TYPE::FLAT:
			_variables["heliostat"][h]["is_xfocus"].value = "FALSE";
			_variables["heliostat"][h]["is_yfocus"].value = "FALSE";
			break;
		case sp_heliostat::FOCUS_TYPE::USER_DEFINED:
			_variables["heliostat"][h]["x_focal_length"].set(helio->user_focal_length);
			//don't break.. also set the x and y focus bools to true
		case sp_heliostat::FOCUS_TYPE::AT_SLANT:
			_variables["heliostat"][h]["is_xfocus"].value = "TRUE";
			_variables["heliostat"][h]["is_yfocus"].value = "TRUE";
			break;
		default:
			break;
		}

		h++;
	}
}

void AutoPilot::update_receivers(var_set &vset, sp_receivers &recs){
	int r=0;
	for(sp_receivers::iterator rec = recs.begin(); rec != recs.end(); rec++){
		_variables["receiver"][r]["rec_type"].set(rec->type);

		_variables["receiver"][r]["rec_offset_x"].set(rec->offset.x);
		_variables["receiver"][r]["rec_offset_y"].set(rec->offset.y);
		_variables["receiver"][r]["rec_offset_z"].set(rec->offset.z);

		_variables["receiver"][r]["absorptance"].set( rec->absorptance );
		_variables["receiver"][r]["therm_loss_base"].set( rec->q_hl_perm2 );
		
		double rw = rec->height / rec->aspect;
		_variables["receiver"][r]["rec_height"].set( rec->height );
		_variables["receiver"][r]["rec_width"].set( rw );
		_variables["receiver"][r]["rec_aspect"].set( rec->aspect ); 
		_variables["receiver"][r]["rec_diameter"].set( rw );
		
		r++;
	}
}

vector<double> AutoPilot::interpolate_vectors(vector<double> &A, vector<double> &B, double alpha)
{
	/* 
	Find the point between A and B, where alpha=0 --> A, and alpha=1 --> B
	*/
	if(A.size() != B.size())
		throw spexception("Error (interpolate_vectors): vectors must have the same dimension.");
	
	vector<double> V;
	for(int i=0; i<(int)A.size(); i++)
		V.push_back(A.at(i) + (B.at(i) - A.at(i))*alpha);

	return V;

}

void AutoPilot::GenerateDesignPointSimulations(sp_ambient &amb, var_set &variables, vector<string> &wdata)
{
	/* 
	Generate the design simulation hours using the SolarPILOT macros. Options must be specified
	directly in the 'variables' data structure. If options are not changed, the default settings
	will be used to generate the simulations (recommended).

	wdata
	------
	This data structure contains a list of all weather entries for the weather file. Each entry is 
	a string formatted with comma separation between weather items. Formatting is:
	day, hour, month,  dni, tdry, pres, wspd
	1..,  0..,  1-12, W/m2,    C,  bar,  m/s
	*/

	amb.weather_data.clear();
	interop::GenerateSimulationWeatherData(variables, LAYOUT_DETAIL::FOR_OPTIMIZATION, wdata);	
	vector<double> stepdat;
	vector<string> sim_step_data = split(variables["solarfield"][0]["sim_step_data"].value, "[P]");
	for(int i=0; i<(int)sim_step_data.size(); i++){
		vector<string> sdata = split(sim_step_data.at(i), ",");
		vector<double> fdata;
		for(int j=0; j<(int)sdata.size(); j++){
			double tt; 
			to_double(sdata.at(j), &tt);
			fdata.push_back(tt);
		}
		//add each step
		amb.AddWeatherStep((int)fdata.at(0), (int)fdata.at(2), fdata.at(1), fdata.at(3), 
			fdata.at(4), fdata.at(6), fdata.at(5), fdata.at(7));
	}

}

void AutoPilot::PreSimCallbackUpdate()
{
	//pass the callback along to the solar field, if applicable
	if(_has_detail_callback){
		_detail_siminfo = _SF->getSimInfoObject();
		_SF->getSimInfoObject()->setCallbackFunction(_detail_callback, _detail_callback_data);
		_SF->getSimInfoObject()->isEnabled(true);
	}
	if(_has_summary_callback){
		if(! _summary_siminfo )
			_summary_siminfo = new simulation_info();
		_summary_siminfo->ResetValues();
		_summary_siminfo->setCallbackFunction(_summary_callback, _summary_callback_data);
	}
}

void AutoPilot::PostProcessLayout()
{
	/* 
	Layout post-process.. collect the layout results and fill the data into the
	layout structure for later use
	*/

	Hvector *hpos = _SF->getHeliostats();
	_layout->heliostat_positions.clear();
	for(int i=0; i<(int)hpos->size(); i++){
		sp_layout::h_position hp;
		//hp.location.Set( *hpos->at(i)->getLocation() );
		hp.location.x = hpos->at(i)->getLocation()->x;
		hp.location.y = hpos->at(i)->getLocation()->y;
		hp.location.z = hpos->at(i)->getLocation()->z;

		hp.cant_vector.i = hpos->at(i)->getCantVector()->i;
        hp.cant_vector.j = hpos->at(i)->getCantVector()->j;
        hp.cant_vector.k = hpos->at(i)->getCantVector()->k;
		
        hp.aimpoint.x = hpos->at(i)->getAimPoint()->x;
		hp.aimpoint.y = hpos->at(i)->getAimPoint()->y;
		hp.aimpoint.z = hpos->at(i)->getAimPoint()->z;

		hp.focal_length = hpos->at(i)->getFocalX();
		hp.template_number = -1;
		hp.user_optics = false;
		_layout->heliostat_positions.push_back( hp );
	}
	_layout->land_area = _SF->getLandObject()->getLandArea() /4046.85642;  //m2->acre

    //Update calculated cost values here
    double sfarea = _SF->getHeliostatArea();
    //set the solar field area
    _layout->area_sf = sfarea;

    Financial *f = _SF->getFinancialObject();

    f->calcPlantCapitalCost( *_SF );
    
    _cost->cost_rec_tot = f->getReceiverCost(); 
    _cost->cost_tower_tot = f->getTowerCost(); 
    _cost->cost_land_tot = f->getLandCost(); 
    _cost->cost_heliostat_tot =  f->getHeliostatCost(); 
    _cost->cost_site_tot = f->getSiteCost(); 
    _cost->cost_plant_tot = f->getPlantCost(); 
    _cost->cost_tes_tot = f->getTESCost(); 
    _cost->cost_fossil_tot = 0.;
    _cost->cost_salestax_tot = f->getSalesTaxCost();
    _cost->cost_direct_tot = f->getTotalDirectCost();
    _cost->cost_epc_tot = 0.;
    _cost->cost_indirect_tot = f->getTotalIndirectCost();
    _cost->cost_installed_tot = f->getTotalInstalledCost();
    
}

void AutoPilot::PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	//simulate flux maps for all of the receivers
	vector<Receiver*> rec_to_sim = *_SF->getReceivers();
	//Get flags and settings
	int fluxmap_format;
	to_integer(_variables["parametric"][0]["fluxmap_format"].value, &fluxmap_format);
	
	if(flux_res_y > 1)
        _SF->getFluxSimObject()->_aim_method = FluxSimData::AIM_STRATEGY::IMAGE_SIZE;
		//_variables["fluxsim"][0]["aim_method"].set( FluxSimData::AIM_STRATEGY::IMAGE_SIZE );

	//Shape the flux surface files to match
	for(unsigned int i=0; i<rec_to_sim.size(); i++){
		rec_to_sim.at(i)->DefineReceiverGeometry(flux_res_x, flux_res_y);	//Flux map should match spec
	}
	
	//------------ 

	vector<int> uday;
	vector<vector<double> > utime;
	Ambient *amb =_SF->getAmbientObject();

	if(! fluxtab.is_user_spacing){
		fluxtab.n_flux_days = 8;
		fluxtab.delta_flux_hrs = 1.;
	}

	Ambient::calcSpacedDaysHours(amb->getPlantLatitude(), amb->getPlantLongitude(), amb->getTimeZone(), 
		fluxtab.n_flux_days, fluxtab.delta_flux_hrs, utime, uday);
	 
	int nflux_sim = 0;
	for(int i=0; i<(int)utime.size(); i++)
		nflux_sim += (int)utime.at(i).size();

	//Arrays to keep track of input values
	fluxtab.azimuths.clear();
	fluxtab.zeniths.clear();

	//trig
	double 
		pi = acos(-1.),
		r2d = 180./pi;

	fluxtab.flux_surfaces.clear();
	//resize the results to accommodate each receiver surface
	int nsurftot=0;
	for(int i=0; i<(int)_SF->getReceivers()->size(); i++){
		for(int j=0; j<(int)_SF->getReceivers()->at(i)->getFluxSurfaces()->size(); j++){
			nsurftot ++;
		}
	}
	fluxtab.flux_surfaces.resize(nsurftot);
	//resize the flux surfaces to match the flux data and the number of annual simulation positions
	for(int i=0; i<nsurftot; i++)
		fluxtab.flux_surfaces.at(i).flux_data.resize(flux_res_y, flux_res_x, nflux_sim);

	int k=0;
	int nday = (int)uday.size();
	for(int i=0; i<nday; i++){
		int nhour_day = (int)utime.at(i).size();
		for(int j=0; j<nhour_day; j++){
			
			_SF->getAmbientObject()->setDateTime(utime.at(i).at(j)+12, (double)uday[i]);
			_SF->getAmbientObject()->calcSunPosition();
			
			//--- keep track of input values
			double az,zen;
			_SF->getAmbientObject()->getSolarPosition(az, zen);
			fluxtab.azimuths.push_back(az);
			fluxtab.zeniths.push_back(zen);
		}
	}

}

void AutoPilot::PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer)
{
	if(! _cancel_simulation){
				
		int itot=0;
		vector<Receiver*> *Recs = _SF->getReceivers();
		int nrec = (int)Recs->size();
		for(int irec = 0; irec<nrec; irec++){
			//how many surfaces on this receiver?
			Receiver *rec = Recs->at(irec);

			int nrecsurf = (int)rec->getFluxSurfaces()->size();
			for( int isurf=0; isurf<nrecsurf; isurf++){
				//transfer data from the result flux map to the sp_flux_table data structure
				fluxmap.flux_surfaces.at(itot).map_name = *rec->getReceiverName() + " surface " + my_to_string(isurf+1);
				FluxSurface *fs = &result.flux_surfaces.at(irec).at(isurf);
				int 
					nflux_x = fs->getFluxNX(),
					nflux_y = fs->getFluxNY();
				FluxGrid *fmap = result.flux_surfaces.at(irec).at(isurf).getFluxMap(); 
				for(int fluxi=0; fluxi<nflux_y; fluxi++){
					for(int fluxj = 0; fluxj < nflux_x; fluxj++){
						//pointers for convenience
						sp_flux_table::sp_flux_stack *fstack = &fluxmap.flux_surfaces.at(itot);
						FluxPoint* fpt = &fmap->at(fluxj).at(nflux_y - fluxi - 1);
						//transfer flux
						fstack->flux_data.at(fluxi, fluxj, flux_layer) = fpt->flux;
						//transfer location
						fstack->xpos.push_back( fpt->location.x );
						fstack->ypos.push_back( fpt->location.y );
					}
				}
					
				itot++;
			}
		}	
	}
}

void AutoPilot::CancelSimulation()
{
	_cancel_simulation = true;
	_SF->CancelSimulation();
}

bool AutoPilot::SimulateFlux(sp_flux_map &fluxmap)
{
	_cancel_simulation = false;

	//Check if a layout is available for simulation
	if(_SF->getHeliostats()->size() == 0){
		// no field layout
		throw spexception("Performance simulation requires an existing layout. Please create a system geometry before simulating performance.");
	}

	//provide an update if we aren't expecting one from the SF object
	/*if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->addSimulationNotice("Simulating receiver flux profile");
	}*/

	sim_result result;
	try{
		Hvector helios = *_SF->getHeliostats();
		_SF->HermiteFluxSimulation(helios);
		result.process_analytical_simulation(*_SF, 2, helios);
	}
	catch( std::exception &e ){
		string emsg = e.what();
		if(_has_summary_callback)
			_summary_siminfo->addSimulationNotice( "Caught an exception during the flux simulation: " + emsg );
		return false;
	}
	catch( ... ){
		if(_has_summary_callback)
			_summary_siminfo->addSimulationNotice( "Caught an unhandled exception during the flux simulation. The simulation terminated unsuccessfully.");
		return false;
	}

	//transfer results data into sp_flux_map
	if(! _cancel_simulation)
		PostProcessFlux(result, fluxmap, 0);
	
	return true;
}

bool AutoPilot::EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max)
{
	/* 
	Create a layout and evaluate the optimization objective function value with as little 
	computation as possible. This method is called by the optimization algorithm.

	The 'obj_metric' is evaluated and set in this algorithm. If the simulation fails, the method 
	returns FALSE.
	*/


	//Update the variable map
	if(opt.is_optimize_rec_height || opt.is_optimize_rec_aspect)
		update_receivers(_variables, recs);
	if(opt.is_optimize_tht || opt.is_optimize_bound)
		update_layout(_variables, layout);
	//update any calculated values
	interop::UpdateCalculatedMapValues(_variables);

	//create the solar field object
	if(! _cancel_simulation){
		_SF->Create(_variables);	if(_SF->ErrCheck()){return false;}
	}
	//Do the layout simulation
	if(! _cancel_simulation){
		if(! CreateLayout(false) )
        {
            CancelSimulation();
            obj_metric = 0.;
            flux_max = 0.;
            return false;
        }
		if(_SF->ErrCheck()){return false;}
	}
	//Do the flux simulation at the design point
	if(! _cancel_simulation){
		//update the flux simulation sun position to match the layout reference point sun position
		double az_des, zen_des;
		if(! 
            _SF->CalcDesignPtSunPosition( _variables["solarfield"][0]["sun_loc_des"].value_int(), az_des, zen_des) 
            ) return false;

		double d2r = acos(-1.)/180.;
		_SF->getAmbientObject()->setSolarPosition( az_des*d2r, zen_des*d2r );
        FluxSimData *fd = _SF->getFluxSimObject();
		fd->_flux_solar_az_in = az_des;	//[deg]
		fd->_flux_solar_el_in = 90.0 - zen_des;
		fd->_flux_time_type = FluxSimData::FLUX_TIME::POSITION;	//sun position specified

		//prep for performance simulation (aim points, etc.)
		interop::PerformanceSimulationPrep(*_SF, _variables, *_SF->getHeliostats(), 0 /*analytical*/);
		
		//do flux simulation
		_SF->HermiteFluxSimulation( *_SF->getHeliostats(), fd->_aim_method == FluxSimData::AIM_STRATEGY::IMAGE_SIZE);	
		if(_SF->ErrCheck()){return false;}		
	}
	
	//get the annual optical power estimate
	double optical_power = _SF->getAnnualPowerApproximation();
	//power cycle efficiency
	double cycle_eff = _SF->getPlantObject()->getCycleEfficiency() * _SF->getPlantObject()->getGrossToNetFactor();
	double power = optical_power * cycle_eff*1.e-6;		//MW-h

	//calculate the total plant cost
	_SF->getFinancialObject()->calcPlantCapitalCost(*_SF);
	double cost = _SF->getFinancialObject()->getTotalInstalledCost();
	
	//Get the maximum flux value
	flux_max=0.;
	for(int i=0; i<_SF->getReceivers()->size(); i++){
		for(int j=0; j<_SF->getReceivers()->at(i)->getFluxSurfaces()->size(); j++){
			double ff = _SF->getReceivers()->at(i)->getFluxSurfaces()->at(j).getMaxObservedFlux();
			if( ff > flux_max )
				flux_max = ff;
		}
	}

	//check to make sure we're producing enough power, otherwise we'll have to penalize
	double qminimum = _SF->getDesignThermalPowerWithLoss();
	double qactual = _SF->getActualThermalPowerWithLoss();
	double power_shortage_ratio = min(qactual/qminimum, 1.);

	//Set the optimization objective value
	double flux_overage_ratio = max(flux_max/opt.flux_max, 1.);
	obj_metric = cost/power 
		* (1. + (flux_overage_ratio - 1.) * opt.flux_penalty) 
		* (1. + (1. - power_shortage_ratio)*opt.power_penalty);

    /*obj_metric = (cost - power*3250)*1.e-7 + 
        max(flux_max - opt.flux_max, 0.)*opt.flux_penalty + 
        max(qminimum - qactual, 0.)*opt.power_penalty;*/

	//Additive? may cause too much interaction
	
	//Set the aiming method back to the original value
	//_variables["fluxsim"][0]["aim_method"].value = my_to_string( aim_method_save );

	//Set the flux resolution back to the original values
	/*_variables["fluxsim"][0]["x_res"].set( flux_x_save );
	_variables["fluxsim"][0]["y_res"].set( flux_y_save );*/

	return true;
}

bool AutoPilot::Optimize(sp_optimize &opt, sp_receivers &recs, sp_layout &layout)
{
	/* 
	
	Optimize
	
	*/

	
	//Create a vector of variables that will be optimized. Create companion vectors that indicate upper and lower feasible ranges for each variable.
    vector<string> names;
    vector<double*> optvars;	//A vector of pointers to the memory locations of the variables being optimized
	vector<double>
		upper_range,	//A vector of upper variable limits
		lower_range;	//A vector of lower variable limits
	vector<bool>
        is_range_constr;
    if(opt.is_optimize_tht){
        names.push_back("THT");
		optvars.push_back( &layout.h_tower );
		if(opt.is_range_constr_tht){
			lower_range.push_back( opt.range_tht[0]/layout.h_tower );
			upper_range.push_back( opt.range_tht[1]/layout.h_tower );
		}
		else
		{
			lower_range.push_back( .5 );
			upper_range.push_back( 2. );
		}
        is_range_constr.push_back(opt.is_range_constr_tht);
	}
	if(opt.is_optimize_rec_aspect){
        names.push_back("Aspect");
		optvars.push_back( &recs.front().aspect);
		if(opt.is_range_constr_aspect){
			lower_range.push_back( opt.range_rec_aspect[0] );
			upper_range.push_back( opt.range_rec_aspect[1] );
		}
		else
		{
			lower_range.push_back( .5 );
			upper_range.push_back( 2. );
		}
        is_range_constr.push_back(opt.is_range_constr_aspect);
	}
	if(opt.is_optimize_rec_height){
        names.push_back("RecHeight");
		optvars.push_back( &recs.front().height);
		if(opt.is_range_constr_rech){
			lower_range.push_back( opt.range_rec_height[0]/recs.front().height );
			upper_range.push_back( opt.range_rec_height[1]/recs.front().height );
		}
		else
		{
			lower_range.push_back( .5 );
			upper_range.push_back( 2. );
		}
        is_range_constr.push_back(opt.is_range_constr_rech);
	}
	if(opt.is_optimize_bound){
        names.push_back("MaxRad");
		optvars.push_back( &layout.land_max);
		if(opt.is_range_constr_bound){
			lower_range.push_back( opt.range_land_bound[0]/layout.land_max );
			upper_range.push_back( opt.range_land_bound[1]/layout.land_max );
		}
		else
		{
			lower_range.push_back( .5 );
			upper_range.push_back( 2. );
		}
        is_range_constr.push_back(opt.is_range_constr_bound);
	}

    switch(opt.method)
    {
    case sp_optimize::METHOD::RSGS:  //Response surface gradient search - original method
	    return Optimize(optvars, upper_range, lower_range, is_range_constr, opt, recs, layout);
        break;
    default:
        return OptimizeAuto( optvars, upper_range, lower_range, is_range_constr, opt, recs, layout, &names);
    }

}

bool AutoPilot::Optimize(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, 
                         sp_optimize &opt, sp_receivers &recs, sp_layout &layout, vector<string> *names)
{
	//Number of variables to be optimized
	int nvars = (int)optvars.size();
	//Store the initial dimensional value of each variable
	vector<double> normalizers;
	for(int i=0; i<nvars; i++)
		normalizers.push_back( *optvars.at(i) );
	
	//the initial normalized point is '1'
	vector<double> current(nvars, 1.);

	bool converged = false;
	int opt_iter = 0;

    int sim_count = 0;

	vector<vector<double> > all_sim_points; //keep track of all of the positions simulated
	vector<double> objective;
	vector<double> max_flux;
	double objective_old=9.e22;
	double objective_new=9.e21;
	int sim_count_begin = 0;

    //Add a formatted simulation notice
    ostringstream os;
    os << "\n\nBeginning Simulation\nIter ";
    for(int i=0; i<(int)optvars.size(); i++)
        os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i)) << "|";
    os << "| Obj.    | Flux";

	_summary_siminfo->addSimulationNotice(os.str());
	while( ! converged ){
		sim_count_begin = objective.size() - 1;	//keep track of the simulation number at the beginning of the main iteration

		//Choose the current point to the the best of all simulations in the previous iteration
		if(opt_iter > 0){
			double zbest = 9.e23;
			int ibest = sim_count_begin;
			for(int i=sim_count_begin; i<(int)objective.size(); i++){
				if(objective.at(i) < zbest){
					zbest = objective.at(i);
					ibest = i;
				}
			}
			current = all_sim_points.at(ibest);
		}

		//----------------------
		//	Current point
		//----------------------

		//Start iteration by evaluating the current point
		_summary_siminfo->addSimulationNotice("--- Iteration " + my_to_string(opt_iter+1) + " ---\n...Simulating base point");
		for(int i=0; i<(int)optvars.size(); i++)
			*optvars.at(i) = current.at(i) * normalizers.at(i);
		all_sim_points.push_back( current );
		double base_obj, base_flux;
		EvaluateDesign(opt, recs, layout, base_obj, base_flux);			
		PostEvaluationUpdate(sim_count++, current, normalizers, base_obj, base_flux);
		if(_cancel_simulation) return false;
		objective.push_back( base_obj );
		max_flux.push_back( base_flux );
		vector<double> surface_objective;
		vector<vector<double> > surface_eval_points;
		surface_objective.push_back( base_obj );
		surface_eval_points.push_back( current );
		//-- 

		//Check to see if no further improvement has been made in the objective function
		objective_new = base_obj;
		if( (objective_old - objective_new)/objective_old < opt.converge_tol ){
			converged = true;

			//Find the best point simulated and return that
			double zbest = 9.e23;
			int ibest = (int)objective.size()-1;	//initialize as the last value just in case
			for(int i=0; i<(int)objective.size(); i++){
				if( objective.at(i) < zbest ){
					zbest = objective.at(i);
					ibest = i;
				}
			}
			for(int i=0; i<(int)optvars.size(); i++)
				*optvars.at(i) = all_sim_points.at(ibest).at(i) * normalizers.at(i);
			
			break;
		}
		objective_old = objective_new;


		//----------------------
		//	Response surface
		//----------------------
		response_surface_data Reg;

		//Generate the set of points required for the response surface characterization
		vector<vector<double> > runs;
		Reg.GenerateSurfaceEvalPoints( current, runs, opt.max_step);

		//Run the evaluation points
		_summary_siminfo->setTotalSimulationCount((int)runs.size());
		if(! _summary_siminfo->addSimulationNotice("...Creating local response surface") ){
            CancelSimulation();
            return false;
        }
		for(int i=0; i<(int)runs.size(); i++){
            if(! _summary_siminfo->setCurrentSimulation(i) ){
                CancelSimulation();
                return false;
            }

			//update the data structures
			for(int j=0; j<(int)optvars.size(); j++)
				*optvars.at(j) = runs.at(i).at(j) * normalizers.at(j);
			
			//Evaluate the design
			double obj, flux;
			all_sim_points.push_back( runs.at(i) );
			EvaluateDesign(opt, recs, layout, obj, flux);
			PostEvaluationUpdate(sim_count++, runs.at(i), normalizers, obj, flux);
			if(_cancel_simulation) return false;
			surface_objective.push_back(obj);
			surface_eval_points.push_back( runs.at(i) );
			objective.push_back( obj);
			max_flux.push_back(flux);
		}

		//construct a bilinear regression model
		_summary_siminfo->addSimulationNotice("...Generating regression fit");
		
		//----------------------
		//	Descent vector
		//----------------------

		Reg.N_vars = nvars;
		Reg.Y = surface_objective;
		Reg.X = surface_eval_points;
		int nbeta = Reg.CalcNumberBetas();
		Reg.Beta.resize( nbeta, 1.);
		//calculate the average Y value
		double yave = 0.;
		for(int i=0; i<(int)Reg.Y.size(); i++)
			yave += Reg.Y.at(i);
		yave *= 1./(double)Reg.Y.size();
		//Set the first beta (the constant) to the average as  an initial guess
		Reg.Beta.front() = yave;
		
		nlopt::opt surf(nlopt::LN_NELDERMEAD, nbeta);	//with higher iteration limits, NELDER MEAD does very well compared to sbplex and cobyla
		surf.set_min_objective( optimize_leastsq_eval, &Reg);
		surf.set_xtol_rel(1.e-7);
		surf.set_ftol_rel(1.e-7);
		surf.set_maxtime(5.);
		
		double min_ss;
		try{
			nlopt::result sres = surf.optimize(Reg.Beta, min_ss);
			
		}
		catch( std::exception &e ){
			_summary_siminfo->addSimulationNotice( e.what() );
			return false;
		}

		//calculate total sum of squares
		double sstot = 0.;
		for(int i=0; i<(int)Reg.Y.size(); i++){
			double ssval = Reg.Y.at(i) - Reg.Beta.front();
			sstot += ssval * ssval;
		}
		//report the coefficient of determination
		_summary_siminfo->addSimulationNotice("... r^2 = " + my_to_string(1.-min_ss/sstot) );
		
		//now we have a response surface described by BETA coefficients. we need to choose the steepest descent
		Reg.ncalls = 0;
		Reg.max_step_size = opt.max_step;
		//nlopt::opt steep(nlopt::LN_COBYLA, nvars);		//optimize with constraint on step size - use COBYLA
		nlopt::opt steep(nlopt::GN_ESCH, nvars);
		steep.set_min_objective( optimize_stdesc_eval, &Reg);
		steep.set_maxtime(2.);
		//add an inequality constraint to find the minimum within a maximum step
		//steep.add_inequality_constraint( optimize_maxstep_eval, &Reg, 1.e-3);
		//add range constraints for the variables
		vector<double>
			range_max, range_min;
		for(int i=0; i<nvars; i++){
			range_max.push_back( fmin(upper_range.at(i), current.at(i) + opt.max_step) );
			range_min.push_back( fmax(lower_range.at(i), current.at(i) - opt.max_step) );
		}

		steep.set_upper_bounds( range_max );
		steep.set_lower_bounds( range_min );

		steep.set_xtol_rel(1.e-4);
		steep.set_ftol_rel(1.e-5);
		
		Reg.cur_pos = current;
		vector<double> stepto(current);
		double min_val;
		nlopt::result dres;
		try{
			dres = steep.optimize(stepto, min_val);
		}
		catch( std::exception &e )
		{
			_summary_siminfo->addSimulationNotice( e.what() );
			return false;
		}
		
		//Calculate the vector from the current point to the point we'd like to step to
		vector<double> step_vector(stepto);
		for(int i=0; i<(int)step_vector.size(); i++)
			step_vector.at(i) += -current.at(i);


		//is the surface regression optimum value better than the best simulated point in the factorial analyis?
		double best_fact_obj = 9.e9;
		int i_best_fact = 0;
		for(int i=0; i<(int)surface_objective.size(); i++){
			if( surface_objective.at(i) < best_fact_obj ){
				best_fact_obj = surface_objective.at(i);
				i_best_fact = i;
			}
		}
		_summary_siminfo->addSimulationNotice("...Best regression objective value = " + my_to_string(min_val) ); 

		//if so, use the best simulated point
		if(best_fact_obj < min_val ){
			_summary_siminfo->addSimulationNotice("...Correcting step direction to use best response surface point.");

			step_vector.resize( stepto.size() );
			for(int i=0; i<(int)current.size(); i++){
				step_vector.at(i) = surface_eval_points.at(i_best_fact).at(i) - current.at(i);
			}
			//correct the step to maintain the maximum step size
			double step_size = 0.;
			for(int i=0; i<(int)step_vector.size(); i++)
				step_size += step_vector.at(i) * step_vector.at(i);
			step_size = sqrt(step_size);
			for(int i=0; i<(int)step_vector.size(); i++)
				step_vector.at(i) *= Reg.max_step_size / step_size;
			//update the minimum value to be the value at the best point
			min_val = best_fact_obj;
		}


		//Check to see whether the projected minimum is significantly better than the current point
		double checktol = (base_obj - min_val)/base_obj;
		if(fabs(checktol) < opt.converge_tol){
			_summary_siminfo->addSimulationNotice(
				"\nConvergence in the objective function value has been achieved. Final step variation: "
				+ my_to_string(checktol) );
			converged = true;
			break;
		}

		
		//----------------------
		//	Steepest descent
		//----------------------

		//move in max steps along the steepest descent vector until the objective function begins to increase
		_summary_siminfo->ResetValues();
		int minmax_iter = 0;
		bool steep_converged = false;
		double prev_obj = base_obj;
		_summary_siminfo->setTotalSimulationCount(opt.max_desc_iter);
		_summary_siminfo->addSimulationNotice("...Moving along steepest descent");
		
		vector<double> start_point = current;
		vector<double> all_steep_objs;
		bool tried_steep_mod = false;
		while( true ){
            if(! _summary_siminfo->setCurrentSimulation(minmax_iter) ){
                CancelSimulation();
                return false;
            }

			//update the variable values
			for(int i=0; i<(int)optvars.size(); i++){
				current.at(i) += step_vector.at(i);
				*optvars.at(i) = current.at(i) * normalizers.at(i);
			}

			//Evaluate the design
			double obj, flux;
			all_sim_points.push_back( current );
			EvaluateDesign(opt, recs, layout, obj, flux);
			PostEvaluationUpdate(sim_count++, current, normalizers, obj, flux);
			if(_cancel_simulation) return false;
			if(minmax_iter > 0)
				prev_obj = objective.back();	//update the latest objective function value
			objective.push_back( obj );
			all_steep_objs.push_back( obj );
			max_flux.push_back( flux );

			minmax_iter++;
			if(minmax_iter >= opt.max_desc_iter)
				break;

			//break here if the objective has increased
			if(obj > prev_obj){
				if(! tried_steep_mod){
					//did the steepest descent at least do better than any of the response surface simulations?
					double best_steep_obj = 9.e9;
					for(int i=0; i<(int)all_steep_objs.size(); i++)
						if( all_steep_objs.at(i) < best_steep_obj ) best_steep_obj = all_steep_objs.at(i);
					if(best_fact_obj < best_steep_obj){
						double zero=0.;
						
						//Calculate a new step vector
						vector<double> new_step_vector( step_vector );

						//go back and try to move in the factorial point direction
						double new_step_size = 0.;
						for(int i=0; i<(int)step_vector.size(); i++){
							double ds = surface_eval_points.at(i_best_fact).at(i) - start_point.at(i);
							new_step_vector.at(i) = ds;
							new_step_size += ds * ds;
						}
						new_step_size = sqrt(new_step_size);

						//check to make sure the new step vector is different from the previous
						double step_diff = 0.;
						for(int i=0; i<(int)step_vector.size(); i++){
							double ds = new_step_vector.at(i) - step_vector.at(i);
							step_diff += ds * ds;
						}
						if( sqrt(step_diff) > opt.max_step/100. && new_step_size > 1.e-8){
							tried_steep_mod = true;
							_summary_siminfo->addSimulationNotice("...Moving back to original point, trying alternate descent direction.");
						
							//correct the step to maintain the maximum step size
							step_vector = new_step_vector;
							double step_size = 0.;
							for(int i=0; i<(int)step_vector.size(); i++)
								step_size += step_vector.at(i) * step_vector.at(i);
							step_size = sqrt(step_size);
							for(int i=0; i<(int)step_vector.size(); i++)
								step_vector.at(i) *= Reg.max_step_size / step_size;
					
							//move back to the starting point
							current = start_point;
							obj = base_obj;
							prev_obj = base_obj;
							continue;
						}
												
					}
				}

				//is the overall steepest descent loop converged?
				if(fabs(obj/prev_obj - 1.) < opt.converge_tol)
					steep_converged = true;
				//Move the current point back to the lowest value
				current = all_sim_points.at( all_sim_points.size() - 2);
				break;
			}

			//break if the step size is very small
			double step_mag = 0.;
			for(int i=0; i<(int)step_vector.size(); i++)
				step_mag += step_vector.at(i) * step_vector.at(i);
			step_mag = sqrt(step_mag);
			if(step_mag < opt.max_step/10.){
				steep_converged = true;
				break;
			}
			
		}
		


		//did we manage to converge the steepest descent in the inner loop? If so, skip the golden section refinement.
		if(steep_converged){
			opt_iter++;
			if( opt_iter >= opt.max_iter )
				break;
			continue;
		}

		/* 
		Now we have isolated the approximate bottom region of the steepest descent curve. Do a golden section in 
		this region to find the true minimum.
		*/
		double golden_ratio = 1./1.61803398875;
		double alpha = 0.;
		int nsimpts = (int)all_sim_points.size();
		vector<double>
				lower_gs = all_sim_points.at( nsimpts - 1 - min( 2, minmax_iter ) ),
				upper_gs = all_sim_points.back(),
				site_a_gs, site_b_gs;
		
		_summary_siminfo->setTotalSimulationCount(opt.max_gs_iter*2);
		_summary_siminfo->addSimulationNotice("...Refining with golden section");

		bool site_a_sim_ok = false;
		bool site_b_sim_ok = false;
		double za, zb;

		for(int gsiter=0; gsiter<opt.max_gs_iter; gsiter++)
		{
            if(! _summary_siminfo->setCurrentSimulation(gsiter*2) ){
                CancelSimulation();
                return false;
            }

			//lower and upper points for golden section
			site_a_gs = interpolate_vectors(lower_gs, upper_gs, 1. - golden_ratio);
			site_b_gs = interpolate_vectors(lower_gs, upper_gs, golden_ratio);
				
			double obj, flux;
			//Evaluate at the lower point
			if(! site_a_sim_ok ){
				current = site_a_gs;
				for(int i=0; i<(int)optvars.size(); i++)
					*optvars.at(i) = current.at(i) * normalizers.at(i);
				all_sim_points.push_back( current );
				EvaluateDesign(opt, recs, layout, obj, flux);			
				PostEvaluationUpdate(sim_count++, current, normalizers, obj, flux);
				if(_cancel_simulation) return false;
				za = obj;
				objective.push_back( obj );
				max_flux.push_back( flux );
			}

            if(! _summary_siminfo->setCurrentSimulation(gsiter*2 + 1) ){
                CancelSimulation();
                return false;
            }

			//Evaluate at the upper point
			if(! site_b_sim_ok){
				current = site_b_gs;
				for(int i=0; i<(int)optvars.size(); i++)
					*optvars.at(i) = current.at(i) * normalizers.at(i);
				all_sim_points.push_back( current );
				EvaluateDesign(opt, recs, layout, obj, flux);			
				PostEvaluationUpdate(sim_count++, current, normalizers, obj, flux);
				if(_cancel_simulation) return false;
				zb = obj;
				objective.push_back( obj );
				max_flux.push_back( flux );
			}

			//if there's no difference between the two objective functions, don't keep iterating
			if( fabs((za - zb)/za) < opt.converge_tol)
				break;

			//Decide how to shift the bounds
			if( gsiter == opt.max_gs_iter -1 ) break;
			if( za > zb ){
				lower_gs = site_a_gs;
				//the lower bound moves up and site a becomes the old site b location
				site_a_sim_ok = true;
				site_b_sim_ok = false;
				za = zb;
			}
			else{
				upper_gs = site_b_gs;
				//the upper bound moves down and site b becomes the old site a location
				site_a_sim_ok = false;
				site_b_sim_ok = true;
				zb = za;
			}
				
		}
		if(_cancel_simulation) return false;
		//update the current point
		current = za < zb ? site_a_gs : site_b_gs;
		opt_iter++;
		if( opt_iter >= opt.max_iter )
			break;
	}
	
	if(_cancel_simulation) return false;

	//redimensionalize the simulation points
	for(int i=0; i<(int)all_sim_points.size(); i++){
		for(int j=0; j<(int)normalizers.size(); j++){
			all_sim_points.at(i).at(j) *= normalizers.at(j);
		}
	}

	_summary_siminfo->ResetValues();


	//Choose the current point to the the best of all simulations in the previous iteration
	double zbest = 9.e23;
	vector<double> best_point;
	int ibest = (int)objective.size()-1;
	for(int i=0; i<(int)objective.size(); i++){
		if(objective.at(i) < zbest){
			zbest = objective.at(i);
			ibest = i;
		}
	}
	best_point = all_sim_points.at(ibest);
	_summary_siminfo->addSimulationNotice("\nBest point found:");
	vector<double> ones(best_point.size(), 1.);
	PostEvaluationUpdate(sim_count++, best_point, ones, zbest, max_flux.at(ibest));

	_summary_siminfo->addSimulationNotice("\n\nOptimization complete!");
	

	//copy the optimization data to the optimization structure
    vector<vector<double> > dimsimpt;
    size_t nr = all_sim_points.size();
    size_t nc = all_sim_points.front().size();

    for(size_t i=0; i<nr; i++){
        if( nc == 0 ) break;
        vector<double> tmp;
        for(size_t j=0; j<nc; j++){
            tmp.push_back( all_sim_points.at(i).at(j) * normalizers.at(j) );
        }
        dimsimpt.push_back(tmp);
    }
	opt.setOptimizationSimulationHistory(dimsimpt, objective, max_flux);

	return true;

}

bool AutoPilot::OptimizeAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, 
                             sp_optimize &opt, sp_receivers &recs, sp_layout &layout, vector<string> *names)
{
    /* 
    Use canned algorithm to optimize
    */

    
    //set up NLOPT algorithm
   
    //map the method
    nlopt::algorithm nlm;
    switch(opt.method)
    {
    case sp_optimize::METHOD::BOBYQA:
        nlm = nlopt::LN_BOBYQA;
        break;
    case sp_optimize::METHOD::COBYLA:
        nlm = nlopt::LN_COBYLA;
        break;
    case sp_optimize::METHOD::NelderMead:
        nlm = nlopt::LN_NELDERMEAD;
        break;
    case sp_optimize::METHOD::NEWOUA:
        nlm = nlopt::LN_NEWUOA;
        break;
    case sp_optimize::METHOD::Subplex:
        nlm = nlopt::LN_SBPLX;
        break;
    }

    nlopt::opt nlobj(nlm, optvars.size() );
    
    //Create optimization helper class
    AutoOptHelper AO;
    AO.Initialize();
    AO.SetObjects( (void*)this,  &recs, &opt, &layout, &nlobj);
    AO.m_opt_vars = optvars;
    //-------
    nlobj.set_min_objective( optimize_auto_eval, &AO  );
    nlobj.set_xtol_rel(1.e-4);
    nlobj.set_ftol_rel(opt.converge_tol);
    nlobj.set_initial_step( vector<double>( optvars.size(), opt.max_step ) );
    nlobj.set_maxeval( opt.max_iter );

    vector<double>
        range_l, 
        range_u;
    for(int i=0; i<(int)lower_range.size(); i++)
        range_l.push_back( is_range_constr.at(i) ? lower_range.at(i) : -1e9 );
    for(int i=0; i<(int)upper_range.size(); i++)
        range_u.push_back( is_range_constr.at(i) ? upper_range.at(i) : 1e9 );
    //nlobj.set_lower_bounds(range_l);
    //nlobj.set_upper_bounds(range_u);
    
    //Number of variables to be optimized
	int nvars = (int)optvars.size();
	//Store the initial dimensional value of each variable
	for(int i=0; i<nvars; i++)
		AO.m_normalizers.push_back( *optvars.at(i) );
	
	//the initial normalized point is '1'
	vector<double> start(nvars, 1.);
    
    //Add a formatted simulation notice
    ostringstream os;
    os << "\n\nBeginning Simulation\nIter ";
    for(int i=0; i<(int)optvars.size(); i++)
        os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i)) << "|";
    os << "| Obj.    | Flux";

    string hmsg = os.str();

    string ol;
    for(int i=0; i<(int)hmsg.size(); i++)
        ol.append("-");

    _summary_siminfo->addSimulationNotice( os.str() );
    _summary_siminfo->addSimulationNotice( ol.c_str() );

    double fmin;
    try{
        nlopt::result resopt = nlobj.optimize( start, fmin );
        
        _summary_siminfo->addSimulationNotice( ol.c_str() );
        
        int iopt = 0;
        double objbest = 9.e9;
        for(int i=0; i<(int)AO.m_all_points.size(); i++){
            double obj = AO.m_objective.at(i);
            if( obj < objbest ){
                objbest = obj;
                iopt = i;
            }
        }

        //write the optimal point found
        ostringstream oo;
        oo << "Best point found:\n";
        for(int i=0; i<(int)optvars.size(); i++)
            oo << (names == 0 ? "" : names->at(i) + "=" ) << setw(8) << AO.m_all_points.at(iopt).at(i) * AO.m_normalizers.at(i) << "   ";
        oo << "\nObjective: " << objbest;
        _summary_siminfo->addSimulationNotice(oo.str() );
    }
    catch(...){
        return false;
    }

    //copy the optimization data to the optimization structure
    vector<vector<double> > dimsimpt;
    size_t nr = AO.m_all_points.size();
    size_t nc = AO.m_all_points.front().size();

    for(size_t i=0; i<nr; i++){
        if( nc == 0 ) break;
        vector<double> tmp;
        for(size_t j=0; j<nc; j++){
            tmp.push_back( AO.m_all_points.at(i).at(j) * AO.m_normalizers.at(j) );
        }
        dimsimpt.push_back(tmp);
    }
    opt.setOptimizationSimulationHistory( dimsimpt, AO.m_objective, AO.m_flux );

    return true;
}


bool AutoPilot::IsSimulationCancelled()
{
	return _cancel_simulation;
}

void AutoPilot::PostEvaluationUpdate(int iter, vector<double> &pos, vector<double> &normalizers, double &obj, double &flux)
{
	ostringstream os;
    os << "[" << setw(2) << iter << "] ";
    for(int i=0; i<(int)pos.size(); i++)
        os << setw(8) << pos.at(i) * normalizers.at(i) << " |";

    os << "|" << setw(8) << obj << " |" << setw(8) << flux;

    _summary_siminfo->addSimulationNotice( os.str() );

}

bool AutoPilot::CalculateFluxMapsOV1(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	overload to provide the flux data in a simple 2D vector arrangement. Each flux map is provided 
	in a continuous sequence, and it is up to the user to separate out the data based on knowledge
	of the number of flux maps and dimension of each flux map.
	*/

	//Call the main algorithm
	sp_flux_table fluxtab_s;
	if(! CalculateFluxMaps(fluxtab_s, flux_res_x, flux_res_y, is_normalized) )
		return false;

	block_t<double> *flux_data = &fluxtab_s.flux_surfaces.front().flux_data;

	//convert data structure
	fluxtab.clear();
	efficiency.clear();
	for(int i=0; i<(int)flux_data->nlayers(); i++){
		sunpos.push_back( vector<double>(2) );
		sunpos.back().at(0) = fluxtab_s.azimuths.at(i);
		sunpos.back().at(1) = fluxtab_s.zeniths.at(i);
		efficiency.push_back( fluxtab_s.efficiency.at(i) );

		for(int j=0; j<flux_res_y; j++){
			vector<double> newline;
			for(int k=0; k<flux_res_x; k++){
				newline.push_back(flux_data->at(j, k, i));
			}
			fluxtab.push_back( newline );
		}
	}

	return true;
}

bool AutoPilot::CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	//override in inherited class
	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
	return false;
}

//---------------- API_S --------------------------
bool AutoPilot_S::CreateLayout(bool do_post_process)
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	PreSimCallbackUpdate();

	int nsim_req = _SF->calcNumRequiredSimulations();

	if(! _SF->isSolarFieldCreated()){
		throw spexception("The solar field Create() method must be called before generating the field layout.");
	}
	if(! _cancel_simulation){
		bool simok = _SF->FieldLayout();			
        
        if(_SF->ErrCheck() || !simok) return false;
	}
	if(do_post_process){
		if(! _cancel_simulation)
			_SF->calcHeliostatShadows();	if(_SF->ErrCheck()){return false;}

		if(! _cancel_simulation)
			PostProcessLayout();
	}

	return true;

}

bool AutoPilot_S::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	_cancel_simulation = false;
	PreSimCallbackUpdate();
	double pi = acos(-1.);
	double d2r = pi/180.;

	//set the solar positions to calculate
	int neff_az;
	int neff_zen;
	
	if(! opttab.is_user_positions){
		//set the solar positions for calculation to the default values
		neff_az = 12;
		opttab.azimuths.clear();
		double eff_az[] = {0.,  30.,  60.,  90., 120., 150., 180., 210., 240., 270., 300., 330.};
		for(int i=0; i<neff_az; i++)
			opttab.azimuths.push_back(eff_az[i]);
		
		neff_zen = 8;
		opttab.zeniths.clear();
		double eff_zen[] = {0.50,   7.,  15.,  30.,  45.,  60.,  75.,  85.};
		for(int i=0; i<neff_zen; i++)
			opttab.zeniths.push_back(eff_zen[i]);
	}
	else{
		neff_az = (int)opttab.azimuths.size();
		neff_zen = (int)opttab.zeniths.size();
	}

	double dni;
	to_double(_variables["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
	
	int neff_tot = neff_az * neff_zen;
	
	_sim_total = neff_tot;	//set the total simulation counter

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}
	
	sim_results results;
	results.resize(neff_tot);
	string neff_tot_str = my_to_string(neff_tot);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			//update the progress counter
			_sim_complete = k;
			
			if(_has_summary_callback)
				if( ! 
					_summary_siminfo->setCurrentSimulation(_sim_complete) 
					) 
					CancelSimulation();
			
			//Update the solar position
			if(! _cancel_simulation)
				_SF->getAmbientObject()->setSolarPosition((opttab.azimuths.at(i)-180.)*d2r, opttab.zeniths.at(j)*d2r);	
			//Update the aim points and images based on the new solar position and tracking angles
			if(! _cancel_simulation)
				interop::AimpointUpdateHandler(*_SF);	
			//Run the performance simulation
			if(! _cancel_simulation)
				_SF->Simulate(args, 4);
			if(! _cancel_simulation)
				results.at(k++).process_analytical_simulation(*_SF, 0);	


			if(_cancel_simulation)
				return false;

		}
	}
	//collect all of the results and process into the efficiency table data structure
	opttab.eff_data.clear();
	k=0;
	for(int j=0; j<neff_zen; j++){
		vector<double> row;
		for(int i=0; i<neff_az; i++){
			row.push_back( results.at(k++).eff_total_sf.ave );
		}
		opttab.eff_data.push_back(row);
	}
	return true;
}

bool AutoPilot_S::CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	Calculate the flux incident on the receiver(s) and surface(s) in the solar field. 

	"fluxtab"	An instance of 'sp_flux_table' that contains information on the method and sun positions used to
				calculate flux maps throughout the year. The class member "flux_surfaces" is a vector of 
				"sp_flux_stack"s that contain a list of flux maps, where each sp_flux_map corresponds to a single
				receiver/surface. For systems with one receiver, there will be one sp_flux_map contained in the 
				flux_surfaces member. 

				Within each sp_flux_stack is a 3-dimensional array called "flux_data" that contains a list of flux
				intensities on the surface (1st and 2nd dimensions) at various sun positions throughout the year
				(3rd dimension). The positions included are specified in the "azimuths" and "zeniths" arrays in 
				the parent sp_flux_table.

	"flux_res_x"
	"flux_res_y"
				These optional parameters specify the flux map data resolution in the horizontal (x) and vertical (y)
				dimensions of each receiver surface.
				Default values are (x=12, y=10)

	"is_normalized"
				This optional argument determines whether flux will be reported on a normalized basis or not. 
				Normalized flux indicates the share of the power delivered to the receiver that is contained within
				a single map element. Non-normalized flux data indicates the actual power delivered in kW/m2 
				based on the reference simulation DNI value, which by default is 950 W/m2.

	*/

	PreSimCallbackUpdate();
	_cancel_simulation = false;
	
	PrepareFluxSimulation(fluxtab, flux_res_x, flux_res_y, is_normalized);
	
	//ambient conditions
	double dni;
	to_double(_variables["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind

	_sim_total = fluxtab.azimuths.size();	//update the expected number of simulations
	_sim_complete = 0;

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating flux maps");
	}

	//From the day and time array, produce an azimuth/zenith array
	fluxtab.efficiency.clear();
	for(int i=0; i<_sim_total; i++){
		_sim_complete++;  //increment

		if(_has_summary_callback)
			if( ! 
				_summary_siminfo->setCurrentSimulation(_sim_complete) 
				) 
				CancelSimulation();

		if(! _cancel_simulation){
			_SF->getAmbientObject()->setSolarPosition( fluxtab.azimuths.at(i), fluxtab.zeniths.at(i) );
			interop::AimpointUpdateHandler(*_SF);	//update the aim points and image properties
		}

		if(! _cancel_simulation)
			_SF->Simulate(args, 4);
		if(! _cancel_simulation)
			_SF->HermiteFluxSimulation( *_SF->getHeliostats() );
			
		sim_result result;
		if(! _cancel_simulation){
			result.process_analytical_simulation(*_SF, 2);	
			fluxtab.efficiency.push_back( result.eff_total_sf.ave );
		}
						
		//Collect flux results here
		if(! _cancel_simulation)
			result.process_flux( _SF, is_normalized);
						
		//Collect the results for each flux surface

		if(! _cancel_simulation){
			PostProcessFlux(result, fluxtab, i);
				
		} //end cancel 

		if(_cancel_simulation)
				return false;
	}
	
	return true;
}

bool AutoPilot_S::CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	PreSimCallbackUpdate();
	return CalculateFluxMapsOV1(sunpos, fluxtab, efficiency, flux_res_x, flux_res_y, is_normalized);
}

//---------------- API_MT --------------------------

#ifdef SP_USE_THREADS

AutoPilot_MT::AutoPilot_MT()
{
	_in_mt_simulation = false;	//initialize
	_cancel_simulation = false;
	_has_summary_callback = false;
	_has_detail_callback = false;
	_summary_callback = 0;
	_detail_callback = 0;
	_summary_callback_data = 0;
	_detail_callback_data = 0;
	_summary_siminfo = 0;
	_SF = 0;
	//initialize with the maximum number of threads
	SetMaxThreadCount(999999);
}

bool AutoPilot_MT::CreateLayout(bool do_post_process)
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	_in_mt_simulation = false;
	PreSimCallbackUpdate();
	try
	{
		//Is it possible to run a multithreaded simulation?
		int nsim_req = _SF->calcNumRequiredSimulations();
		if(_has_detail_callback){
			_detail_siminfo->ResetValues();
			_detail_siminfo->setTotalSimulationCount(nsim_req);
			_detail_siminfo->addSimulationNotice("Creating field layout");
		}

		if(_n_threads > 1 && nsim_req > 1){
			//More than 1 thread and more than 1 simulation to run

			//Prepare the master solar field object for layout simulation
			WeatherData wdata;
			bool full_sim = _SF->PrepareFieldLayout(*_SF, wdata);
		
			//If full simulation is required...
			if(full_sim){

				int nthreads = min(nsim_req, _n_threads);

				//update progress
				if(_has_detail_callback)
					_detail_siminfo->addSimulationNotice("Preparing " + my_to_string(_n_threads) + " threads for simulation");
				
				
				//Duplicate SF objects in memory
				SolarField **SFarr;
				SFarr = new SolarField*[nthreads];
				for(int i=0; i<nthreads; i++){
					SFarr[i] = new SolarField(*_SF);
				}
			
				//Create sufficient results arrays in memory
				sim_results results;
				results.resize(nsim_req);
						
				//Calculate the number of simulations per thread
				int npert = (int)ceil((float)nsim_req/(float)nthreads);

				//Create thread objects
				_simthread = new LayoutSimThread[nthreads];
				_n_threads_active = nthreads;	//Keep track of how many threads are active
				_in_mt_simulation = true;
			
				int
					sim_first = 0,
					sim_last = npert;
				for(int i=0; i<nthreads; i++){
					_simthread[i].Setup(my_to_string(i+1), SFarr[i], &_variables, &results, &wdata, sim_first, sim_last, false, false);
					sim_first = sim_last;
					sim_last = min(sim_last+npert, nsim_req);
				}
				
				if(_has_detail_callback){
					_detail_siminfo->setTotalSimulationCount(nsim_req);
					_detail_siminfo->setCurrentSimulation(0);
					_detail_siminfo->addSimulationNotice("Simulating layout design-point hours...");
				}
				
				//Run
				for(int i=0; i<nthreads; i++)
					thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

				//Wait loop
				while(true){
					int nsim_done = 0, nsim_remain=0, nthread_done=0;
					for(int i=0; i<nthreads; i++){
						if( _simthread[i].IsFinished() )
							nthread_done ++;
					
						int ns, nr;
						_simthread[i].GetStatus(&ns, &nr);
						nsim_done += ns;
						nsim_remain += nr;
					
					
					}
					_sim_total = nsim_req;
					_sim_complete = nsim_done;

					if(_has_detail_callback){
						if(! _detail_siminfo->setCurrentSimulation(nsim_done) )
                            break;
                    }
					
				
					if(nthread_done == nthreads) break;
					std::this_thread::sleep_for(std::chrono::milliseconds(75));

				}

				//Check to see whether the simulation was cancelled
				bool cancelled = false;
				for(int i=0; i<nthreads; i++){
					cancelled = cancelled || _simthread[i].IsSimulationCancelled();
				}
			    //check to see whether simulation errored out
                bool errored_out = false;
                for(int i=0; i<_n_threads; i++){
                    errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
                }
                if( errored_out )
                {
                    CancelSimulation();
                    //Get the error messages, if any
                    string errmsgs;
                    for(int i=0; i<_n_threads; i++){
                        for(int j=0; j<_simthread[i].GetSimMessages()->size(); j++)
                            errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
                    }
                    //Display error messages
                    if(! errmsgs.empty() && _has_summary_callback)
                        _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
                }

	            //Clean up dynamic memory
	            for(int i=0; i<_n_threads; i++){
		            delete SFarr[i];
	            }
	            delete [] SFarr;
	            delete [] _simthread;
	            _simthread = 0;

	            //If the simulation was cancelled per the check above, exit out
	            if(cancelled || errored_out){
		            return false;
	            }
			
				//For the map-to-annual case, run a simulation here
				if(_variables["solarfield"][0]["des_sim_detail"].value_int() == LAYOUT_DETAIL::MAP_TO_ANNUAL)	
					if(! _cancel_simulation)
						SolarField::AnnualEfficiencySimulation(_variables, *_SF, results); 

				//Process the results
				if(! _cancel_simulation)
					_SF->ProcessLayoutResults(&results, nsim_req);

			}
		}
		else{
			_n_threads_active = 1;
			_in_mt_simulation = false;

			if(! _cancel_simulation){
				bool simok = _SF->FieldLayout();			
            
                if(_SF->ErrCheck() || !simok) return false;
            }
		}
		if(do_post_process){
			if(! _cancel_simulation)
				_SF->calcHeliostatShadows();	if(_SF->ErrCheck()){return false;}
			if(! _cancel_simulation)
				PostProcessLayout();
		}
	}
	catch(std::exception &e){
		_summary_siminfo->addSimulationNotice(e.what());
		return false;
	}
	catch(...){
		_summary_siminfo->addSimulationNotice("Caught unhandled exception in layout simulation. Simulation unsuccessful.");
		return false;
	}
	return true;
}

bool AutoPilot_MT::SetMaxThreadCount(int nt)
{
	//check to make sure the max number of threads is less
	//than the machine's capacity
	try{
		unsigned int nmax = std::thread::hardware_concurrency();
		_n_threads = min(max(nt,1), (int)nmax);
	}
	catch(...)
	{
		return false;
	}
	return true;
}

bool AutoPilot_MT::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	
	_cancel_simulation = false;
	PreSimCallbackUpdate();
	double pi = acos(-1.);
	double d2r = pi/180.;

	//set the solar positions to calculate
	int neff_az;
	int neff_zen;
	
	if(! opttab.is_user_positions){
		//set the solar positions for calculation to the default values
		neff_az = 12;
		opttab.azimuths.clear();
		double eff_az[] = {0.,  30.,  60.,  90., 120., 150., 180., 210., 240., 270., 300., 330.};
		for(int i=0; i<neff_az; i++)
			opttab.azimuths.push_back(eff_az[i]);
		
		neff_zen = 8;
		opttab.zeniths.clear();
		double eff_zen[] = {0.50,   7.,  15.,  30.,  45.,  60.,  75.,  85.};
		for(int i=0; i<neff_zen; i++)
			opttab.zeniths.push_back(eff_zen[i]);
	}
	else{
		neff_az = (int)opttab.azimuths.size();
		neff_zen = (int)opttab.zeniths.size();
	}

	double dni;
	to_double(_variables["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
	
	int neff_tot = neff_az * neff_zen;
	
	_sim_total = neff_tot;	//set the total simulation counter

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}

	//load the sun positions into a matrix_t
	matrix_t<double> sunpos(neff_tot, 2);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			sunpos.at(k,0) = (opttab.azimuths.at(i) - 180.)*d2r;
			sunpos.at(k++,1) = opttab.zeniths.at(j)*d2r;
		}
	}

	//------------do the multithreaded run----------------
	
	//Create copies of the solar field
	SolarField **SFarr;
	SFarr = new SolarField*[_n_threads];
	for(int i=0; i<_n_threads; i++){
		SFarr[i] = new SolarField(*_SF);
	}

	//Create sufficient results arrays in memory
	sim_results results;
	results.resize(_sim_total);
						
	//Calculate the number of simulations per thread
	int npert = (int)ceil((float)_sim_total/(float)_n_threads);

	//Create thread objects
	_simthread = new LayoutSimThread[_n_threads];
	_n_threads_active = _n_threads;	//Keep track of how many threads are active
				
	int
		sim_first = 0,
		sim_last = npert;
	for(int i=0; i<_n_threads; i++){
		_simthread[i].Setup(my_to_string(i), SFarr[i], &_variables, &results, &sunpos, args, sim_first, sim_last, true, false);
		sim_first = sim_last;
		sim_last = min(sim_last+npert, _sim_total);
	}
	//Run
	for(int i=0; i<_n_threads; i++)
		thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

	//Wait loop
	while(true){
		int nsim_done = 0, nsim_remain=0, nthread_done=0;
		for(int i=0; i<_n_threads; i++){
			if( _simthread[i].IsFinished() )
				nthread_done ++;
					
			int ns, nr;
			_simthread[i].GetStatus(&ns, &nr);
			nsim_done += ns;
			nsim_remain += nr;
					
					
		}
		if(_has_summary_callback){
			if( ! _summary_siminfo->setCurrentSimulation(nsim_done) )
				CancelSimulation();
		}
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
    
    //check to see whether simulation errored out
    bool errored_out = false;
    for(int i=0; i<_n_threads; i++){
        errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
    }
    if( errored_out )
    {
        CancelSimulation();
        //Get the error messages, if any
        string errmsgs;
        for(int i=0; i<_n_threads; i++){
            for(int j=0; j<_simthread[i].GetSimMessages()->size(); j++)
                errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
        }
        //Display error messages
        if(! errmsgs.empty() && _has_summary_callback)
            _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
    }

	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled || errored_out){
		return false;
	}

	//collect all of the results and process into the efficiency table data structure
	opttab.eff_data.clear();
	k=0;
	for(int j=0; j<neff_zen; j++){
		vector<double> row;
		for(int i=0; i<neff_az; i++){
			row.push_back( results.at(k++).eff_total_sf.ave );
		}
		opttab.eff_data.push_back(row);
	}

	return true;
}

bool AutoPilot_MT::CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	Calculate the flux incident on the receiver(s) and surface(s) in the solar field. 

	"fluxtab"	An instance of 'sp_flux_table' that contains information on the method and sun positions used to
				calculate flux maps throughout the year. The class member "flux_surfaces" is a vector of 
				"sp_flux_stack"s that contain a list of flux maps, where each sp_flux_map corresponds to a single
				receiver/surface. For systems with one receiver, there will be one sp_flux_map contained in the 
				flux_surfaces member. 

				Within each sp_flux_stack is a 3-dimensional array called "flux_data" that contains a list of flux
				intensities on the surface (1st and 2nd dimensions) at various sun positions throughout the year
				(3rd dimension). The positions included are specified in the "azimuths" and "zeniths" arrays in 
				the parent sp_flux_table.

	"flux_res_x"
	"flux_res_y"
				These optional parameters specify the flux map data resolution in the horizontal (x) and vertical (y)
				dimensions of each receiver surface.
				Default values are (x=12, y=10)

	"is_normalized"
				This optional argument determines whether flux will be reported on a normalized basis or not. 
				Normalized flux indicates the share of the power delivered to the receiver that is contained within
				a single map element. Non-normalized flux data indicates the actual power delivered in kW/m2 
				based on the reference simulation DNI value, which by default is 950 W/m2.

	*/


	_cancel_simulation = false;
	PreSimCallbackUpdate();
	PrepareFluxSimulation(fluxtab, flux_res_x, flux_res_y, is_normalized);
	
	//ambient conditions
	double dni;
	to_double(_variables["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind

	_sim_total = fluxtab.azimuths.size();	//update the expected number of simulations
	_sim_complete = 0;

	//collect the sun positions for the simulations into a single matrix_t
	matrix_t<double> sunpos(_sim_total, 2);
	for(int i=0; i<_sim_total; i++){
		sunpos.at(i,0) = fluxtab.azimuths.at(i);
		sunpos.at(i,1) = fluxtab.zeniths.at(i);
	}
	fluxtab.efficiency.clear();
	fluxtab.efficiency.resize(_sim_total);

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating flux maps");
	}


	//------------do the multithreaded run----------------
	
	//Create copies of the solar field
	SolarField **SFarr;
	SFarr = new SolarField*[_n_threads];
	for(int i=0; i<_n_threads; i++){
		SFarr[i] = new SolarField(*_SF);
	}

	//Create sufficient results arrays in memory
	sim_results results;
	results.resize(_sim_total);

	//Calculate the number of simulations per thread
	int npert = (int)ceil((float)_sim_total/(float)_n_threads);

	//Create thread objects
	_simthread = new LayoutSimThread[_n_threads];
	_n_threads_active = _n_threads;	//Keep track of how many threads are active
	
	int
		sim_first = 0,
		sim_last = npert;
	for(int i=0; i<_n_threads; i++){
		_simthread[i].Setup(my_to_string(i), SFarr[i], &_variables, &results, &sunpos, args, sim_first, sim_last, true, true);
		_simthread[i].IsFluxmapNormalized(is_normalized);
		sim_first = sim_last;
		sim_last = min(sim_last+npert, _sim_total);
	}
	//Run
	for(int i=0; i<_n_threads; i++)
		thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

	//Wait loop
	while(true){
		int nsim_done = 0, nsim_remain=0, nthread_done=0;
		for(int i=0; i<_n_threads; i++){
			if( _simthread[i].IsFinished() )
				nthread_done ++;
					
			int ns, nr;
			_simthread[i].GetStatus(&ns, &nr);
			nsim_done += ns;
			nsim_remain += nr;
					
					
		}
		if(_has_summary_callback){
			if( ! _summary_siminfo->setCurrentSimulation(nsim_done) ) 
				CancelSimulation();
		}
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
	//check to see whether simulation errored out
    bool errored_out = false;
    for(int i=0; i<_n_threads; i++){
        errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
    }
    if( errored_out )
    {
        CancelSimulation();
        //Get the error messages, if any
        string errmsgs;
        for(int i=0; i<_n_threads; i++){
            for(int j=0; j<_simthread[i].GetSimMessages()->size(); j++)
                errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
        }
        //Display error messages
        if(! errmsgs.empty() && _has_summary_callback)
            _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
    }

	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled || errored_out){
		return false;
	}

	for(int i=0; i<_sim_total; i++){
		PostProcessFlux(results.at(i), fluxtab, i);
		fluxtab.efficiency.at(i) = results.at(i).eff_total_sf.ave;
	}


	return true;
}

bool AutoPilot_MT::CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	PreSimCallbackUpdate();
	return CalculateFluxMapsOV1(sunpos, fluxtab, efficiency, flux_res_x, flux_res_y, is_normalized);
}

void AutoPilot_MT::CancelSimulation()
{
	CancelMTSimulation();
}

void AutoPilot_MT::CancelMTSimulation()
{
	_cancel_simulation = true;
	if(_in_mt_simulation && _simthread != 0){
		for(int i=0; i<_n_threads_active; i++){
			_simthread[i].CancelSimulation();
		}
	}
}

#endif // SP_USE_THREADS

