#include "AutoPilot_API.h"
#include "LayoutSimulateThread.h"
#include "IOUtil.h"

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
	vector<double> Y;
	vector<vector<double> > X;
	vector<double> cur_pos;
	vector<double> Beta;
	int ncalls;
	double big_M;
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
		double ib=0;
		for(int i=0; i<N_vars+1; i++){
			double xi = i==0 ? 1. : xpt.at(i-1);
			for(int j=i; j<N_vars+1; j++){
				double xj = j==0 ? 1. : xpt.at(j-1);
				yret += xi * xj * Beta.at(ib++);
			}
		}

		return yret;
	};
	
};


double optimize_leastsq_eval(unsigned n, const double *x, double *grad, void *data)
{
	/* 
	Evaluate the sum of squares of the fit
	*/
	
	response_surface_data *D = static_cast<response_surface_data*>( data );
	D->ncalls ++;
	//We are solving for the Beta coefficients (contained in X)
	if(D->Beta.size() != n){
		D->Beta.resize(n,1.);
	}

	for(int i=0; i<n; i++)
		D->Beta.at(i) = x[i];
	
	double ss=0.;
	double ave=0.;
	for(int i=0; i<(int)D->X.size(); i++){
		double y = D->EvaluateBiLinearResponse(D->X.at(i));
		ave += y;
		double ssv = (y - D->Y.at(i));
		ss += ssv * ssv;
	}

	return ss;

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
	for(int i=0; i<n; i++){
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
	for(int i=0; i<n; i++){
		xpt.push_back(x[i]);
		double xistep = x[i] - D->cur_pos.at(i);
		//D->cur_pos.at(i) = x[i];

		ssize += xistep*xistep;
	}
	ssize = sqrt(ssize);
	return ssize - D->max_step_size;

}

void TestQuadSurface(vector<double*> &vptrs, double &opt, double &flux)
{
	//something random but regressable.
	opt = 0.;
	for(int i=0; i<(int)vptrs.size(); i++)
		opt += sin(*vptrs.at(i) );
		//opt += i * (*vptrs.at(i) ) + (*vptrs.at(i))*(*vptrs.at(max(i-1,0)));
	flux = 2 * opt;

};

AutoPilot::AutoPilot()
{
	_has_callback = false;
	_is_solarfield_external = false;
	_SF = 0;
	_callback = 0;
	_callback_data = 0;
	_local_siminfo = 0;
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

	if( _local_siminfo != 0 && _has_callback && !_is_deep_callback)
	{
		//quietly try to delete the simulation info object
		try{
			delete _local_siminfo;
		}
		catch(...){}
	}
	return;
}

bool AutoPilot::CreateLayout()
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

void AutoPilot::SetCallback( void (*callback)(simulation_info* siminfo, void *data), void *cdata, bool is_deep_callback)
{
	_has_callback = true;
	_callback = callback;
	_callback_data = cdata;
	_is_deep_callback = is_deep_callback;
}

void AutoPilot::SetExternalSFObject( SolarField *SF )
{
	_SF = SF;
	_is_solarfield_external = true;
}

bool AutoPilot::Setup(sp_ambient &ambient, sp_cost &cost, sp_layout &layout, sp_heliostats &helios, sp_receivers &recs){

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

	//Create the solar field object
	_SF->Create(_variables);
	
	//pass the callback along to the solar field, if applicable
	if(_has_callback && _is_deep_callback){
		_local_siminfo = _SF->getSimInfoObject();
		_SF->getSimInfoObject()->setCallbackFunction(_callback, _callback_data);
		_SF->getSimInfoObject()->isEnabled(true);
	}
	if(_has_callback && !_is_deep_callback){
		_local_siminfo = new simulation_info();
		_local_siminfo->ResetValues();
		_local_siminfo->setCallbackFunction(_callback, _callback_data);
	}
	_setup_ok = true;
	
	return true;
}

void AutoPilot::SetupExpert(var_set &vset)
{
	_cancel_simulation = false;

	//Dynamically allocate the solar field object, if needed
	if(! _is_solarfield_external ){
		_SF = new SolarField();
	}
	_variables = vset;	//copy
	_SF->Create(_variables);

	if(_has_callback && _is_deep_callback){
		_SF->getSimInfoObject()->setCallbackFunction(_callback, _callback_data);
		_SF->getSimInfoObject()->isEnabled(true);
	}
	_setup_ok = true;
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
			if( i == ambient.user_atten_coefs.size() ) avals.append(",");
		}
		
		_variables["ambient"][0]["atm_model"].value = "2";
		_variables["ambient"][0]["atm_model"].cselect = 2;
		_variables["ambient"][0]["atm_coefs"].value = avals;
		_variables["ambient"][0]["atm_coefs"].choices.at(2) = avals;
		break;
	default:
		if( _has_callback ){
			_local_siminfo->addSimulationNotice("Invalid atmospheric model number provided. Options are 0=Delsol clear day, 1=Delsol hazy day, 2=user coefs");
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
		if( _has_callback )
			_local_siminfo->addSimulationNotice("The specified sun shape model is invalid. Options are "
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
}

void AutoPilot::update_layout(var_set &vset, sp_layout &layout){
	//set the one-off values
	vset["solarfield"][0]["q_des"].value = my_to_string(layout.q_design);
	vset["solarfield"][0]["accept_max"].value = my_to_string(layout.span_cw);
	vset["solarfield"][0]["accept_min"].value = my_to_string(layout.span_ccw);
	vset["solarfield"][0]["tht"].value = my_to_string(layout.h_tower);

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
		if( _has_callback )
			_local_siminfo->addSimulationNotice("The specified land bound type is invalid. Options are Scaled=0, Fixed=1, Polygon=2.");
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
		_variables["heliostat"][h]["width"].value = my_to_string(helio->width);
		_variables["heliostat"][h]["height"].value = my_to_string(helio->height);
		_variables["heliostat"][h]["n_cant_x"].value = my_to_string(helio->npanels_w);
		_variables["heliostat"][h]["n_cant_y"].value = my_to_string(helio->npanels_h);
		_variables["heliostat"][h]["err_elevation"].value = "0.";
		_variables["heliostat"][h]["err_azimuth"].value = "0.";
		_variables["heliostat"][h]["err_surface_x"].value = my_to_string(helio->optical_error);
		_variables["heliostat"][h]["err_surface_y"].value = my_to_string(helio->optical_error);
		_variables["heliostat"][h]["err_reflect_x"].value = "0.";
		_variables["heliostat"][h]["err_reflect_y"].value = "0.";
		_variables["heliostat"][h]["reflectivity"].value = my_to_string(helio->reflectance);
		_variables["heliostat"][h]["soiling"].value = "1.";

		//Canting
		_variables["heliostat"][h]["cant_method"].value = my_to_string(helio->cant_type);
		switch (helio->focus_type)
		{
		case sp_heliostat::CANT_TYPE::FLAT:
		case sp_heliostat::CANT_TYPE::AT_SLANT:
			break;
		case sp_heliostat::CANT_TYPE::AT_DAY_HOUR:
			_variables["heliostat"][h]["cant_day"].value = my_to_string(helio->cant_settings.point_day);
			_variables["heliostat"][h]["cant_hour"].value = my_to_string(helio->cant_settings.point_hour);
			break;
		case sp_heliostat::CANT_TYPE::USER_VECTOR:
			_variables["heliostat"][h]["is_cant_vect_slant"].value = helio->cant_settings.scale_with_slant ? "TRUE" : "FALSE";
			_variables["heliostat"][h]["cant_vect_i"].value = my_to_string(helio->cant_settings.point_vector.i);
			_variables["heliostat"][h]["cant_vect_j"].value = my_to_string(helio->cant_settings.point_vector.j);
			_variables["heliostat"][h]["cant_vect_k"].value = my_to_string(helio->cant_settings.point_vector.k);
			break;
		default:
			break;
		}

		//Focusing
		_variables["heliostat"][h]["focus_method"].value = my_to_string(helio->focus_type);
		_variables["heliostat"][h]["is_focal_equal"].value = "TRUE";
		switch (helio->focus_type)
		{
		case sp_heliostat::FOCUS_TYPE::FLAT:
			_variables["heliostat"][h]["is_xfocus"].value = "FALSE";
			_variables["heliostat"][h]["is_yfocus"].value = "FALSE";
			break;
		case sp_heliostat::FOCUS_TYPE::USER_DEFINED:
			_variables["heliostat"][h]["x_focal_length"].value = my_to_string(helio->user_focal_length);
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
		_variables["receiver"][r]["rec_type"].value = my_to_string(rec->type);

		_variables["receiver"][r]["rec_offset_x"].value = my_to_string(rec->offset.x);
		_variables["receiver"][r]["rec_offset_y"].value = my_to_string(rec->offset.y);
		_variables["receiver"][r]["rec_offset_z"].value = my_to_string(rec->offset.z);

		_variables["receiver"][r]["absorptance"].value = my_to_string( rec->absorptance );
		_variables["receiver"][r]["therm_loss_base"].value = my_to_string( rec->q_hl_perm2 );
		
		double rw = rec->height / rec->aspect;
		_variables["receiver"][r]["rec_width"].value = my_to_string( rw );
		_variables["receiver"][r]["rec_aspect"].value = my_to_string( rec->aspect ); 
		_variables["receiver"][r]["rec_diameter"].value = my_to_string( rw );
		
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
	interop::GenerateSimulationWeatherData(variables, LAYOUT_DETAIL::AVG_PROFILES, wdata);	//LAYOUT_DETAIL::SINGLE_POINT
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
		hp.location.Set( *hpos->at(i)->getLocation() );
		hp.cant_vector.Set( *hpos->at(i)->getCantVector() );
		hp.aimpoint.Set( *hpos->at(i)->getAimPoint() );
		hp.focal_length = hpos->at(i)->getFocalX();
		hp.template_number = -1;
		hp.user_optics = false;
		_layout->heliostat_positions.push_back( hp );
	}

}

void AutoPilot::PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	//simulate flux maps for all of the receivers
	vector<Receiver*> rec_to_sim = *_SF->getReceivers();
	//Get flags and settings
	int fluxmap_format;
	to_integer(_variables["parametric"][0]["fluxmap_format"].value, &fluxmap_format);
	
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
	if(_has_callback && !_is_deep_callback){
		_local_siminfo->ResetValues();
		_local_siminfo->addSimulationNotice("Simulating receiver flux profile");
	}

	sim_result result;
	try{
		Hvector helios = *_SF->getHeliostats();
		_SF->HermiteFluxSimulation(helios);
		result.process_analytical_simulation(*_SF, 2, helios);
	}
	catch( std::exception &e ){
		string emsg = e.what();
		if(_has_callback)
			_local_siminfo->addSimulationNotice( "Caught an exception during the flux simulation: " + emsg );
		return false;
	}
	catch( ... ){
		if(_has_callback)
			_local_siminfo->addSimulationNotice( "Caught an unhandled exception during the flux simulation. The simulation terminated unsuccessfully.");
		return false;
	}

	//transfer results data into sp_flux_map
	if(! _cancel_simulation)
		PostProcessFlux(result, fluxmap, 0);
	
	return true;
}

void AutoPilot::GenerateSurfaceEvalPoints( vector<double> &point, vector<vector<double> > &sim_points, double tolerance)
{
	/* 
	Take a current point (vector of doubles) that is normalized, and calculate a design of experiments run that 
	would produce a linear estimate of the local surface. Parameters are varied up/down by the tolerance. The
	runs required are returned in the "sim_points" array.
	*/
	
	int nvars = (int)point.size();

	sim_points.clear();

	int nruns = pow(2.0, (double)nvars);

	vector<int> divisors;
	for(int i=0; i<nvars; i++)
		divisors.push_back( pow(2.0, (double)i) );

	vector<vector<int> > design;
	design.push_back( vector<int>(nvars, 1) );
	
	for(int i=1; i<nruns; i++){
		vector<int> newline;
		for(int j=0; j<nvars; j++){
			//newline.push_back( point.at(j) * (1. + tolerance * (sim_points.at(i-1).at(j) * (fmod( i, divisors.at(j)) == 0 ? -1 : 1) ) ) );
			newline.push_back( design.at(i-1).at(j) * (int)(fmod((double)i, (double)divisors.at(j) ) == 0 ? -1 : 1 ) );
		}
		design.push_back(newline);
	}

	for(int i=0; i<nruns; i++){
		sim_points.push_back( vector<double>(nvars) );
		for(int j=0; j<nvars; j++){
			sim_points.at(i).at(j) = point.at(j) * (1. + tolerance * design.at(i).at(j) );
		}
	}
		
}

bool AutoPilot::EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max){
	//override in inherited class
	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
	return false;
}

bool AutoPilot::Optimize(sp_optimize &opt, sp_receivers &recs, sp_layout &layout)
{
	/* 
	
	Optimize
	
	*/

	
	//Create a vector of variables that will be optimized. Create companion vectors that indicate upper and lower feasible ranges for each variable.
	vector<double*> optvars;	//A vector of pointers to the memory locations of the variables being optimized
	vector<double>
		upper_range,	//A vector of upper variable limits
		lower_range;	//A vector of lower variable limits
	if(opt.is_optimize_tht){
		optvars.push_back( &layout.h_tower );
		if(opt.is_range_constr_tht){
			lower_range.push_back( opt.range_tht[0]/layout.h_tower );
			upper_range.push_back( opt.range_tht[1]/layout.h_tower );
		}
		else
		{
			lower_range.push_back( .1 );
			upper_range.push_back( 10. );
		}
	}
	if(opt.is_optimize_rec_aspect){
		optvars.push_back( &recs.front().aspect);
		if(opt.is_range_constr_aspect){
			lower_range.push_back( opt.range_rec_aspect[0] );
			upper_range.push_back( opt.range_rec_aspect[1] );
		}
		else
		{
			lower_range.push_back( .1 );
			upper_range.push_back( 10. );
		}
	}
	if(opt.is_optimize_rec_height){
		optvars.push_back( &recs.front().height);
		if(opt.is_range_constr_rech){
			lower_range.push_back( opt.range_rec_height[0]/recs.front().height );
			upper_range.push_back( opt.range_rec_height[1]/recs.front().height );
		}
		else
		{
			lower_range.push_back( .1 );
			upper_range.push_back( 10. );
		}
	}
	if(opt.is_optimize_bound){
		optvars.push_back( &layout.land_max);
		if(opt.is_range_constr_bound){
			lower_range.push_back( opt.range_land_bound[0]/layout.land_max );
			upper_range.push_back( opt.range_land_bound[1]/layout.land_max );
		}
		else
		{
			lower_range.push_back( .1 );
			upper_range.push_back( 10. );
		}
	}
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

	vector<vector<double> > all_sim_points; //keep track of all of the positions simulated
	vector<double> objective;
	vector<double> max_flux;

	while( ! converged ){

		
		vector<vector<double> > runs;


		//Start iteration by evaluating the current point
		_local_siminfo->addSimulationNotice("[Iter " + my_to_string(opt_iter+1) + "] Simulating base point");
		for(int i=0; i<(int)optvars.size(); i++)
			*optvars.at(i) = current.at(i) * normalizers.at(i);
		all_sim_points.push_back( current );
		double base_obj, base_flux;
		EvaluateDesign(opt, recs, layout, base_obj, base_flux);			
		objective.push_back( base_obj );
		max_flux.push_back( base_flux );
		//-- 

		//Generate the set of points required for the response surface characterization
		GenerateSurfaceEvalPoints( current, runs, opt.max_step_size);

		//Run the evaluation points
		_local_siminfo->setTotalSimulationCount((int)runs.size());
		_local_siminfo->addSimulationNotice("[Iter " + my_to_string(opt_iter+1) + "]" + "Creating local response surface");
		for(int i=0; i<(int)runs.size(); i++){
			_local_siminfo->setCurrentSimulation(i);

			//update the data structures
			for(int j=0; j<(int)optvars.size(); j++)
				*optvars.at(j) = runs.at(i).at(j) * normalizers.at(j);

			//Evaluate the design
			double obj, flux;
			all_sim_points.push_back( current );
			EvaluateDesign(opt, recs, layout, obj, flux);
			
			objective.push_back( obj);
			max_flux.push_back(flux);
		}

		//construct a bilinear regression model
		response_surface_data Reg;
		Reg.N_vars = nvars;
		Reg.Y = objective;
		Reg.X = runs;
		int nbeta = Reg.CalcNumberBetas();
		Reg.Beta.resize( nbeta, .1);
		nlopt::opt surf(nlopt::LN_SBPLX, nbeta);		//subplex seems to work better than COBYLA
		surf.set_min_objective( optimize_leastsq_eval, &Reg);
		surf.set_xtol_rel(1.e-4);

		double min_dummy;
		try{
			nlopt::result sres = surf.optimize(Reg.Beta, min_dummy);
			
			converged = true;
		}
		catch( std::exception &e ){
			_local_siminfo->addSimulationNotice( e.what() );
			return false;
		}

		
		//now we have a response surface described by BETA coefficients. we need to choose the steepest descent
		Reg.ncalls = 0;
		Reg.max_step_size = opt.max_step_size;
		Reg.big_M = 1000*(*std::max_element(Reg.Beta.begin(), Reg.Beta.end()) );
		nlopt::opt steep(nlopt::LN_COBYLA, nvars);		//optimize with constraint on step size - use COBYLA
		steep.set_min_objective( optimize_stdesc_eval, &Reg);
		//add an inequality constraint to find the minimum within a maximum step
		steep.add_inequality_constraint( optimize_maxstep_eval, &Reg, 1.e-3);
		//add range constraints for the variables
		steep.set_upper_bounds( upper_range );
		steep.set_lower_bounds( lower_range );

		steep.set_xtol_rel(1.e-4);
		
		Reg.cur_pos = current;
		vector<double> stepto(current);
		double min_val;
		nlopt::result dres;
		try{
			dres = steep.optimize(stepto, min_val);
		}
		catch( std::exception &e )
		{
			_local_siminfo->addSimulationNotice( e.what() );
			return false;
		}
		
		//Check to see whether the regression surface minimum is significantly better than the current point
		if((base_obj - Reg.EvaluateBiLinearResponse(stepto))/base_obj < opt.converge_tol){
			converged = true;
			break;
		}

		//Calculate the vector
		vector<double> step_vector(stepto);
		for(int i=0; i<(int)step_vector.size(); i++)
			step_vector.at(i) += -current.at(i);

		
		//move in max steps along the steepest descent vector until the objective function begins to increase
		int minmax_iter = 0;
		bool steep_converged = false;
		while( true ){
			//update the variable values
			for(int i=0; i<(int)optvars.size(); i++){
				current.at(i) += step_vector.at(i);
				*optvars.at(i) = current.at(i) * normalizers.at(i);
			}

			//Evaluate the design
			double obj, flux;
			all_sim_points.push_back( current );
			EvaluateDesign(opt, recs, layout, obj, flux);
			double prev_obj = objective.back();
			objective.push_back( obj );
			max_flux.push_back( flux );

			//update the local steepest gradient
			Reg.cur_pos = current;
			stepto = current;	//initialize
			minmax_iter++;

			//break here if the objective has increased
			if(obj > prev_obj){
				//is the overall steepest descent loop converged?
				if(abs(obj/prev_obj - 1.) < opt.converge_tol)
					steep_converged = true;
				break;
			}

			try{
				dres = steep.optimize(stepto, min_val);
			}
			catch( std::exception &e )
			{
				_local_siminfo->addSimulationNotice( e.what() );
				return false;
			}

			//update the steepest descent vector
			for(int i=0; i<(int)step_vector.size(); i++)
				step_vector.at(i) = stepto.at(i) - current.at(i);

			//break if the step size is very small
			double step_mag = 0.;
			for(int i=0; i<(int)step_vector.size(); i++)
				step_mag += step_vector.at(i) * step_vector.at(i);
			step_mag = sqrt(step_mag);
			if(step_mag < opt.max_step_size/10.){
				steep_converged = true;
				break;
			}
			
				
		}
		//did we manage to converge the steepest descent in the inner loop? If so, break.
		if(steep_converged)
			break;

		/* 
		Now we have isolated the approximate bottom region of the steepest descent curve. Do a golden section in 
		this region to find the true minimum.
		*/
		double golden_ratio = 1./1.61803398875;
		double alpha = 0.;
		int nsimpts = (int)all_sim_points.size();
		vector<double>
				lower_gs = all_sim_points.at( nsimpts - min( 3, minmax_iter ) ),
				upper_gs = all_sim_points.back(),
				site_a_gs, site_b_gs;

		for(int gsiter=0; gsiter<5; gsiter++)
		{
			//lower and upper points for golden section
			site_a_gs = interpolate_vectors(lower_gs, upper_gs, golden_ratio);
			site_b_gs = interpolate_vectors(lower_gs, upper_gs, 1. - golden_ratio);
				
			double za, zb;
			double obj, flux;
			//Evaluate at the lower point
			current = site_a_gs;
			for(int i=0; i<(int)optvars.size(); i++)
				*optvars.at(i) = current.at(i) * normalizers.at(i);
			all_sim_points.push_back( current );
			EvaluateDesign(opt, recs, layout, obj, flux);			
			za = obj;
			objective.push_back( obj );
			max_flux.push_back( flux );

			//Evaluate at the upper point
			current = site_b_gs;
			for(int i=0; i<(int)optvars.size(); i++)
				*optvars.at(i) = current.at(i) * normalizers.at(i);
			all_sim_points.push_back( current );
			EvaluateDesign(opt, recs, layout, obj, flux);			
			zb = obj;
			objective.push_back( obj );
			max_flux.push_back( flux );

			//Decide how to shift the bounds
			if( za > zb ){
				lower_gs = site_a_gs;
			}
			else{
				upper_gs = site_b_gs;
			}
				
		}

		//update the current point
		current = interpolate_vectors(lower_gs, upper_gs, 0.5);
				
		opt_iter++;
	}

	return true;

}

//---------------- API_S --------------------------
bool AutoPilot_S::CreateLayout()
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	
	_local_siminfo->addSimulationNotice("Creating solar field layout...");

	if(! _SF->isSolarFieldCreated()){
		throw spexception("The solar field Create() method must be called before generating the field layout.");
	}
	if(! _cancel_simulation){
		_SF->FieldLayout();			if(_SF->ErrCheck()){return false;}
	}

	if(! _cancel_simulation)
		_SF->calcHeliostatShadows();	if(_SF->ErrCheck()){return false;}

	if(! _cancel_simulation)
		PostProcessLayout();

	return true;
}

bool AutoPilot_S::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	_cancel_simulation = false;
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

	if(_has_callback){
		_local_siminfo->ResetValues();
		_local_siminfo->setTotalSimulationCount(_sim_total);
		_local_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}
	
	sim_results results;
	results.resize(neff_tot);
	string neff_tot_str = my_to_string(neff_tot);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			//update the progress counter
			_sim_complete = k;
			
			if(_has_callback)
				_local_siminfo->setCurrentSimulation(_sim_complete);
			
			//Update the solar position
			if(! _cancel_simulation)
				_SF->getAmbientObject()->setSolarPosition((opttab.azimuths.at(i)-180.)*d2r, opttab.zeniths.at(j)*d2r);	
			//Update the aim points and images based on the new solar position and tracking angles
			if(! _cancel_simulation)
				interop::AimpointUpdateHandler(*_SF, _variables);	
			//Run the performance simulation
			if(! _cancel_simulation)
				_SF->Simulate(args, 4);
			if(! _cancel_simulation)
				results.at(k++).process_analytical_simulation(*_SF, 2);	


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


	_cancel_simulation = false;
	
	PrepareFluxSimulation(fluxtab, flux_res_x, flux_res_y, is_normalized);
	
	//ambient conditions
	double dni;
	to_double(_variables["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind

	_sim_total = fluxtab.azimuths.size();	//update the expected number of simulations
	_sim_complete = 0;

	if(_has_callback){
		_local_siminfo->ResetValues();
		_local_siminfo->setTotalSimulationCount(_sim_total);
		_local_siminfo->addSimulationNotice("Simulating flux maps");
	}

	//From the day and time array, produce an azimuth/zenith array
	for(int i=0; i<_sim_total; i++){
		_sim_complete++;  //increment

		if(_has_callback)
			_local_siminfo->setCurrentSimulation(_sim_complete);

		if(! _cancel_simulation){
			_SF->getAmbientObject()->setSolarPosition( fluxtab.azimuths.at(i), fluxtab.zeniths.at(i) );
			interop::AimpointUpdateHandler(*_SF, _variables);	//update the aim points and image properties
		}

		if(! _cancel_simulation)
			_SF->Simulate(args, 4);
		if(! _cancel_simulation)
			_SF->HermiteFluxSimulation( *_SF->getHeliostats() );
			
		sim_result result;
		if(! _cancel_simulation)
			result.process_analytical_simulation(*_SF, 2);	//TO DO: #2
						
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

bool AutoPilot_S::EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max)
{
	/* 
	Create a layout and evaluate the optimization objective function value with as little 
	computation as possible. This method is called by the optimization algorithm.

	The 'obj_metric' is evaluated and set in this algorithm. If the simulation fails, the method 
	returns FALSE.
	*/


	//----- temp for testing -----
	vector<double*> optvars;
	optvars.push_back( &layout.h_tower );
	optvars.push_back( &recs.front().aspect );
	optvars.push_back( &recs.front().height );
	optvars.push_back( &layout.land_max );

	TestQuadSurface(optvars, obj_metric, flux_max);
	return true;
	//------------------------------


	_cancel_simulation = false;
	
	//make sure the aiming strategy is simple aimpoints
	int aim_method_save = _variables["fluxsim"][0]["aim_method"].value_int();
	//_variables["fluxsim"][0]["aim_method"].set( Flux::AIM_STRATEGY::SIMPLE );

	//set the receiver flux surfaces to the correct resolution to balance run time with accuracy
	int flux_x_save = _variables["fluxsim"][0]["x_res"].value_int();
	int flux_y_save = _variables["fluxsim"][0]["y_res"].value_int();
	int flux_x, flux_y;
	if( _variables["receiver"][0]["rec_type"].value_int() == 0 ){
		//external receiver
		flux_x = 10;
		flux_y = 11;
	}
	else{
		//flat plate receiver
		flux_x = 11;
		flux_y = 11;
	}
	_variables["fluxsim"][0]["x_res"].set( flux_x );
	_variables["fluxsim"][0]["y_res"].set( flux_y );

	//Update the variable map
	if(opt.is_optimize_rec_height || opt.is_optimize_rec_aspect)
		update_receivers(_variables, recs);
	if(opt.is_optimize_tht || opt.is_optimize_bound)
		update_layout(_variables, layout);

	//create the solar field object
	if(! _cancel_simulation){
		_SF->Create(_variables);	if(_SF->ErrCheck()){return false;}
	}
	//Do the layout simulation
	if(! _cancel_simulation){
		_SF->FieldLayout();			if(_SF->ErrCheck()){return false;}
	}
	//Do the flux simulation at the design point
	if(! _cancel_simulation){
		//update the flux simulation sun position to match the layout reference point sun position
		double pos[2];
		_SF->getSunPositionDesign(pos);
		double d2r = acos(-1.)/180.;
		_SF->getAmbientObject()->setSolarPosition( pos[0]*d2r, (90.-pos[1])*d2r );
		_variables["fluxsim"][0]["flux_solar_az_in"].set( pos[0] );	//[deg]
		_variables["fluxsim"][0]["flux_solar_el_in"].set( pos[1] );
		_variables["fluxsim"][0]["flux_time_type"].set( 0 );	//sun position specified

		//prep for performance simulation (aim points, etc.)
		interop::PerformanceSimulationPrep(*_SF, _variables, *_SF->getHeliostats(), 0 /*analytical*/);
		
		//do flux simulation
		_SF->HermiteFluxSimulation( *_SF->getHeliostats(), _variables["fluxsim"][0]["aim_method"].value_int() == Flux::AIM_STRATEGY::IMAGE_SIZE);	
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
	
	obj_metric = cost/power;
	
	//Get the maximum flux value
	flux_max=0.;
	for(int i=0; i<_SF->getReceivers()->size(); i++){
		for(int j=0; j<_SF->getReceivers()->at(i)->getFluxSurfaces()->size(); j++){
			double ff = _SF->getReceivers()->at(i)->getFluxSurfaces()->at(j).getMaxObservedFlux();
			if( ff > flux_max )
				flux_max = ff;
		}
	}

	
	//Set the aiming method back to the original value
	_variables["fluxsim"][0]["aim_method"].value = my_to_string( aim_method_save );

	//Set the flux resolution back to the original values
	_variables["fluxsim"][0]["x_res"].set( flux_x_save );
	_variables["fluxsim"][0]["y_res"].set( flux_y_save );
}


//---------------- API_MT --------------------------

#ifdef SP_USE_THREADS

AutoPilot_MT::AutoPilot_MT()
{
	_in_mt_simulation = false;	//initialize
	_cancel_simulation = false;
	_callback = 0;
	_callback_data = 0;
	_local_siminfo = 0;
	//initialize with the maximum number of threads
	SetMaxThreadCount(3); //999999);
}

bool AutoPilot_MT::CreateLayout()
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	_in_mt_simulation = false;
	
	try
	{
		//Is it possible to run a multithreaded simulation?
		int nsim_req = _SF->calcNumRequiredSimulations();
		if(_has_callback){
			_local_siminfo->ResetValues();
			_local_siminfo->setTotalSimulationCount(nsim_req);
			_local_siminfo->addSimulationNotice("Creating field layout");
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
				if(_has_callback)
					_local_siminfo->addSimulationNotice("Preparing " + my_to_string(_n_threads) + " threads for simulation");
				
				
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
					_simthread[i].Setup(SFarr[i], &_variables, &results, &wdata, sim_first, sim_last, false, false);
					sim_first = sim_last;
					sim_last = min(sim_last+npert, nsim_req);
				}
				
				if(_has_callback){
					_local_siminfo->setTotalSimulationCount(nsim_req);
					_local_siminfo->setCurrentSimulation(0);
					_local_siminfo->addSimulationNotice("Simulating layout design-point hours...");
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

					if(_has_callback)
						_local_siminfo->setCurrentSimulation(nsim_done);
					
				
					if(nthread_done == nthreads) break;
					std::this_thread::sleep_for(std::chrono::milliseconds(75));

				}

				//Check to see whether the simulation was cancelled
				bool cancelled = false;
				for(int i=0; i<nthreads; i++){
					cancelled = cancelled || _simthread[i].IsSimulationCancelled();
				}
			
				//Clean up dynamic memory
				for(int i=0; i<nthreads; i++){
					delete SFarr[i];
				}
				delete [] SFarr;
				delete [] _simthread;
				_simthread = 0;

				//If the simulation was cancelled per the check above, exit out
				if(cancelled){
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
			if(! _cancel_simulation)
				_SF->FieldLayout();			if(_SF->ErrCheck()){return false;}
		}
		if(! _cancel_simulation)
			_SF->calcHeliostatShadows();	if(_SF->ErrCheck()){return false;}
		if(! _cancel_simulation)
			PostProcessLayout();

	}
	catch(std::exception &e){
		_local_siminfo->addSimulationNotice(e.what());
		return false;
	}
	catch(...){
		_local_siminfo->addSimulationNotice("Caught unhandled exception in layout simulation. Simulation unsuccessful.");
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

	if(_has_callback){
		_local_siminfo->ResetValues();
		_local_siminfo->setTotalSimulationCount(_sim_total);
		_local_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}

	//load the sun positions into a matrix_t
	matrix_t<double> sunpos(neff_tot, 2);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			sunpos.at(k,0) = opttab.azimuths.at(i)*d2r;
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
		_simthread[i].Setup(SFarr[i], &_variables, &results, &sunpos, args, sim_first, sim_last, true, false);
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
		if(_has_callback)
			_local_siminfo->setCurrentSimulation(nsim_done);
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
			
	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled){
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

	if(_has_callback){
		_local_siminfo->ResetValues();
		_local_siminfo->setTotalSimulationCount(_sim_total);
		_local_siminfo->addSimulationNotice("Simulating flux maps");
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
		_simthread[i].Setup(SFarr[i], &_variables, &results, &sunpos, args, sim_first, sim_last, true, true);
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
		if(_has_callback)
			_local_siminfo->setCurrentSimulation(nsim_done);
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
			
	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled){
		return false;
	}

	for(int i=0; i<_sim_total; i++)
		PostProcessFlux(results.at(i), fluxtab, i);


	return true;
}

bool AutoPilot_MT::EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max){
	
	
	return false;
}

void AutoPilot_MT::CancelMTSimulation()
{
	_cancel_simulation = true;
	if(_in_mt_simulation){
		for(int i=0; i<_n_threads_active; i++){
			_simthread[i].CancelSimulation();
		}
	}
}

#endif // SP_USE_THREADS

