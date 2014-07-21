#include "interop.h"
#include "SolarField.h"
#include "STObject.h"
#include "solpos00.h"
#include <stdio.h>
#include "sort_method.h"

#include <sstream>


template<typename T> std::string to_string( T value )
{
	std::ostringstream os;
	os << value;
	return os.str();
}

//-------------------  arraystring ----------------

ArrayString::ArrayString(){data.clear();}
	
//wxArrayStr &operator=( ArrayString &array );
ArrayString &ArrayString::operator=( vector<string> &array )
{ 
	data = array; 
	return *this;
}
	
int ArrayString::size(){return (int)data.size();}
string ArrayString::operator[](int i){return data.at(i);}
string& ArrayString::at(int i){return data.at(i);}
	
void ArrayString::clear(){data.clear();}
void ArrayString::Clear(){data.clear();}	

void ArrayString::resize(int newsize){data.resize(newsize);}
void ArrayString::push_back(string value){data.push_back(value);}
void ArrayString::Add(string value){data.push_back(value);}

string ArrayString::back(){return data.back();}
int ArrayString::Index(string item){
	for(int i=0; i<(int)data.size(); i++){
		if(item == data.at(i)) return i;
	}
	return -1;
};

vector<string>::iterator ArrayString::erase(vector<string>::iterator position){ return data.erase( position ); }
vector<string>::iterator ArrayString::begin(){return data.begin(); }
//-------------------

//-------------------  par_variable ----------------
par_variable::par_variable(){ linked = false; layout_required = false;}
//-------------------


void interop::UpdateCalculatedMapValues(var_set &V){
	
	/* 
	This method takes the variable set "V" and completes all of the calculations required to fill in 
	parameter settings that are needed to begin simulation. The method uses certain base parameters
	to calculate other input parameters.. e.g. power block gross power is a calculated value based on 
	solar field design, solar multiple, efficiencty, etc.

	This method is called by UpdateCalculatedGUIValues(). This method does not set any GUI input values. 
	All changes to the GUI are handled in the UpdateCalculatedGUIValues method.
	*/

	double tht; to_double(V["solarfield"][0]["tht"].value, &tht);
	double pi=acos(-1.);
	double Pi=pi;
	double d2r = pi/180.;
	
	//For all heliostat pages---
	for(map<int, var_map>::iterator it = V["heliostat"].begin(); it != V["heliostat"].end(); it++){
		int ind = it->first;
		
		//Heliostat collision radius
		double hm, wm;
		to_double(V["heliostat"][ind]["height"].value, &hm);
		to_double(V["heliostat"][ind]["width"].value, &wm);
		double dc = 2. * sqrt( pow(hm/2., 2) + pow(wm/2., 2) );
		V["heliostat"][ind]["r_collision"].value = to_string( dc);
		
		//Heliostat cant radius
		int cant_type = V["heliostat"][ind]["cant_method"].value_int(); 
			/* 
			No canting=0
			On-axis at slant=-1
			On-axis, user-defined=1
			Off-axis, day and hour=3
			User-defined vector=4 
			*/
		if(cant_type == 1){	//On-axis, user-defined
			bool is_scaled = V["heliostat"][ind]["is_cant_rad_scaled"].value_bool(); 
			double cant_radius, 
				cant_rad_scaled = V["heliostat"][ind]["cant_rad_scaled"].value_double();
			
			if(is_scaled){ cant_radius = cant_rad_scaled * tht; }
			else{ cant_radius = cant_rad_scaled; }
			V["heliostat"][ind]["cant_radius"].value = to_string( cant_radius );
		}
		else if(cant_type == 3){	//Off-axis, day and hour
			/* Calculate the sun position at this day and hour */
			double cant_day, cant_hour, lat, lon, tmz;
			to_double( V["heliostat"][ind]["cant_day"].value, &cant_day);
			to_double( V["heliostat"][ind]["cant_hour"].value, &cant_hour);
			to_double( V["ambient"][0]["latitude"].value, &lat);
			to_double( V["ambient"][0]["longitude"].value, &lon);
			to_double( V["ambient"][0]["time_zone"].value, &tmz);

			DateTime DT;
			double month, dom;
			DT.hours_to_date((cant_day-1)*24+cant_hour+12, month, dom);

			//Instantiate the solpos object
			struct posdata SP, *pdat;
			pdat = &SP;	//point to structure for convenience
			S_init(pdat);		//Initialize the values
	
			pdat->latitude = float(lat);		//[deg] {float} North is positive
			pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
			pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
			pdat->year = 2011;		//[year] {int} 4-digit year
			pdat->month = month;	//[mo] {int} (1-12)
			pdat->day = dom;		//[day] {int} Day of the month
			pdat->daynum = cant_day;	//[day] {int} Day of the year
			pdat->hour = cant_hour+12;		//[hr] {int} 0-23
			pdat->minute = 0;	//[min] {int} 0-59
			pdat->second = 0;	//[sec]	{int} 0-59
			pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


			long retcode = 0;		//Initialize with no errors
			retcode = S_solpos(pdat);	//Call the solar posotion algorithm
			S_decode(retcode, pdat);	//Check the return code

			/* Check to see if the time/day entered is below sunset. If so, notify the user */
			DT.SetHour(12);
			DT.SetDate(2011,month,dom);
			DT.SetYearDay(cant_day+1);
			double hrs[2];
			//Calculate the daytime hours
			Ambient::calcDaytimeHours(hrs, lat*d2r, lon*d2r, tmz, DT);
			hrs[0] += -12.;
			hrs[1] += -12.;

			V["heliostat"][ind]["cant_sun_el"].value = to_string( 90. - SP.zenetr);
			V["heliostat"][ind]["cant_sun_az"].value = to_string( SP.azim );

		}
		else if(cant_type == 4){	//User-defined vector
			//Calculate the magnitude of the vector components
			double i, j, k, scale, cmag;
			to_double(V["heliostat"][ind]["cant_vect_i"].value, &i);
			to_double(V["heliostat"][ind]["cant_vect_j"].value, &j);
			to_double(V["heliostat"][ind]["cant_vect_k"].value, &k);
			cmag = sqrt( pow(i, 2) + pow(j, 2) + pow(k, 2) );
			V["heliostat"][ind]["cant_norm_i"].value = to_string( i/cmag );
			V["heliostat"][ind]["cant_norm_j"].value = to_string( j/cmag );
			V["heliostat"][ind]["cant_norm_k"].value = to_string( k/cmag );
			to_double(V["heliostat"][ind]["cant_vect_scale"].value, &scale);
			V["heliostat"][ind]["cant_mag_i"].value = to_string( i/cmag*scale );
			V["heliostat"][ind]["cant_mag_j"].value = to_string( j/cmag*scale );
			V["heliostat"][ind]["cant_mag_k"].value = to_string( k/cmag*scale );
		}

		//calculate the total error
		double err_elevation, err_azimuth, err_surface_x, err_surface_y, err_reflect_x, err_reflect_y;
		to_double(V["heliostat"][ind]["err_elevation"].value, &err_elevation);
		to_double(V["heliostat"][ind]["err_azimuth"].value, &err_azimuth);
		to_double(V["heliostat"][ind]["err_surface_x"].value, &err_surface_x);
		to_double(V["heliostat"][ind]["err_surface_y"].value, &err_surface_y);
		to_double(V["heliostat"][ind]["err_reflect_x"].value, &err_reflect_x);
		to_double(V["heliostat"][ind]["err_reflect_y"].value, &err_reflect_y);
		double err_tot = sqrt( pow(2.*err_elevation, 2) + pow(2.*err_azimuth, 2) + pow(2.*err_surface_x, 2) + 
			pow(2.*err_surface_y, 2) + pow(err_reflect_x, 2) + pow(err_reflect_y, 2));
		V["heliostat"][ind]["err_total"].value = to_string( err_tot );

		//Reflectance
		double ref, soil;
		to_double(V["heliostat"][ind]["reflectivity"].value, &ref);
		to_double(V["heliostat"][ind]["soiling"].value, &soil);
		V["heliostat"][ind]["ref_total"].value = to_string( ref*soil );

		//Aperture area
		bool 
			//is_faceted = lower_case(V["heliostat"][ind]["is_faceted"].value) == "true" ? true : false,
			is_round = V["heliostat"][ind]["is_round"].cselect == 1 ? true : false;	//0=Rectangular, 1=Round
		double n_cant_x, n_cant_y, x_gap, y_gap, reflect_ratio, dm;
		to_double(V["heliostat"][ind]["reflect_ratio"].value, &reflect_ratio);
		
		double a_tot;
		if(! is_round ){
			to_double(V["heliostat"][ind]["n_cant_x"].value, &n_cant_x);
			to_double(V["heliostat"][ind]["n_cant_y"].value, &n_cant_y);
			to_double(V["heliostat"][ind]["x_gap"].value, &x_gap);
			to_double(V["heliostat"][ind]["y_gap"].value, &y_gap);
			if(n_cant_x < 1.) n_cant_x = 1.;
			if(n_cant_y < 1.) n_cant_y = 1.;
			a_tot = hm * wm * reflect_ratio - x_gap * hm * (n_cant_x - 1.) - y_gap * wm * (n_cant_y - 1.) + (n_cant_x-1.)*(n_cant_y-1.)*x_gap*y_gap;
		}
		else{
			to_double(V["heliostat"][ind]["diameter"].value, &dm);
			a_tot = Pi * pow(dm/2., 2) * reflect_ratio;
		}
		V["heliostat"][ind]["a_total"].value = to_string( a_tot );

	}
	//--end of all helistat templates----



	//For all receiver pages---
	double rec_area_total = 0.;
	for(map<int, var_map>::iterator it = V["receiver"].begin(); it != V["receiver"].end(); it++){
	
		//receiver aspect ratio and 
		//Receiver area
		double width, diam, height;
		to_double(it->second["rec_width"].value, &width);
		to_double(it->second["rec_diameter"].value, &diam);
		to_double(it->second["rec_height"].value, &height);
		int rec_type;
		to_integer(it->second["rec_type"].value, &rec_type);
		double aspect, area;
		if(rec_type == 0){
			//External receiver
			aspect = height/diam;
			area = acos(-1.)*diam*height;
		}
		else if(rec_type == 1){
			//cavity
			aspect = height/width;
			double rec_cav_rad, rec_cav_cdepth;
			to_double(it->second["rec_cav_rad"].value, &rec_cav_rad);
			to_double(it->second["rec_cav_cdepth"].value, &rec_cav_cdepth);
			area = rec_cav_rad * 2.* acos(rec_cav_cdepth/rec_cav_rad) * height;	//area should be scaled according to the arc length of the cavity
			//check to see if the cavity curvature radius is less than the aperture width. if so, warn the user
			if(rec_cav_rad*2.<width){
				it->second["rec_cav_rad"].value = to_string( width/2. );
			}


		}
		else{
			//flat plate
			aspect = height/width;
			area = height*width;
		}
		it->second["rec_aspect"].value = to_string(aspect);
		it->second["absorber_area"].value = to_string(area);
		rec_area_total += area;	//keep track of the total receiver area for later

		//receiver optical height
		double zoff;
		to_double(it->second["rec_offset_z"].value, &zoff);
		it->second["optical_height"].value = to_string(tht+zoff);


		//Estimated heat loss
		vector<string>
			tvals = split(it->second["therm_loss_load"].value, ",");
		int nt = tvals.size();
			
		vector<double> tval(nt);
		for(int i=0; i<nt; i++){ to_double(tvals.at(i), &tval.at(i)); }
		double tp = 0.;
		for(int i=0; i<nt; i++){ tp += tval.at(i); }
		
		double therm_loss_base = it->second["therm_loss_base"].value_double();
		it->second["therm_loss"].value = to_string(therm_loss_base * area/1.e3 * tp);

		//Piping loss
		it->second["piping_loss"].value = to_string( (it->second["piping_loss_coef"].value_double() * tht + it->second["piping_loss_const"].value_double())/1.e3, "%.3f" );
		


	}
	//-----------end of receiver pages---
	

	//----- plant page -----------
	//Gross electric output
	double eta, sm, qdes;
	to_double(V["solarfield"][0]["q_des"].value, &qdes);
	to_double(V["plant"][0]["solar_mult"].value, &sm);
	to_double(V["plant"][0]["eta_cycle"].value, &eta);
	double pgr = qdes / sm * eta;
	V["plant"][0]["power_gross"].value = to_string(pgr);

	//net electric output
	double fact, pnet;
	to_double(V["plant"][0]["par_factor"].value, &fact);
	pnet = pgr * fact;
	V["plant"][0]["power_net"].value = to_string( pnet );
	
	//Min and max optimization levels
	double qmin, qmax;
	to_double(V["solarfield"][0]["q_des_opt_min"].value, &qmin);
	to_double(V["solarfield"][0]["q_des_opt_max"].value, &qmax);
	qmin *= eta/sm;
	qmax *= eta/sm;
	V["plant"][0]["power_net_min"].value = to_string(qmin);
	V["plant"][0]["power_net_max"].value = to_string(qmax);
	V["plant"][0]["q_sf_des_display"].value = to_string(pgr/eta*sm);
	V["plant"][0]["q_pb_des_display"].value = to_string(pgr/eta);


	//--------end of plant page-----
	

	//----- Costs/financials page -----------
	double tower_fixed_cost, tower_exp, tower_cost;
	to_double(V["financial"][0]["tower_fixed_cost"].value, &tower_fixed_cost);
	to_double(V["financial"][0]["tower_exp"].value, &tower_exp);
	tower_cost = tower_fixed_cost * exp( tht * tower_exp );
	V["financial"][0]["tower_cost"].value = to_string( tower_cost );
	
	double rec_ref_cost, rec_ref_area, rec_cost_exp, rec_cost;
	to_double(V["financial"][0]["rec_ref_cost"].value, &rec_ref_cost);
	to_double(V["financial"][0]["rec_ref_area"].value, &rec_ref_area);
	to_double(V["financial"][0]["rec_cost_exp"].value, &rec_cost_exp);
	rec_cost = rec_ref_cost * pow( rec_area_total / rec_ref_area, rec_cost_exp );
	V["financial"][0]["rec_cost"].value = to_string( rec_cost );

	double plant_cost, plant_spec_cost;
	to_double(V["financial"][0]["plant_spec_cost"].value, &plant_spec_cost);
	plant_cost = plant_spec_cost * pgr;
	V["financial"][0]["plant_cost"].value = to_string( plant_cost );

	double tes_cost, hours_tes, tes_spec_cost;
	to_double(V["financial"][0]["tes_spec_cost"].value, &tes_spec_cost);
	to_double(V["plant"][0]["hours_tes"].value, &hours_tes);
	tes_cost = hours_tes * qdes * 1000. * tes_spec_cost;
	V["financial"][0]["tes_cost"].value = to_string( tes_cost );


	//-- flux simulation page --
	//may need to calculate the solar position for a particular day/time, otherwise set the solar position 
	//to the input value
	double az,zen;
	if (V["fluxsim"][0]["flux_time_type"].value == "0"){
		//Sun position are input, just set the corresponding values
		to_double(V["fluxsim"][0]["flux_solar_az_in"].value, &az); 
		az *= d2r;
		to_double(V["fluxsim"][0]["flux_solalr_el_in"].value, &zen); 
		zen *= d2r;	//convert to radians
		V["fluxsim"][0]["flux_solar_az"].value = to_string(az);
		V["fluxsim"][0]["flux_solar_el"].value = to_string(zen);
	}
	else{
		//hour/day are provided, calculate the solar position
		double flux_day, flux_month, flux_hour, lat, lon, tmz;
		to_double( V["fluxsim"][0]["flux_day"].value, &flux_day);	//Day of the month
		to_double( V["fluxsim"][0]["flux_month"].value, &flux_month);	//month of the year
		to_double( V["fluxsim"][0]["flux_hour"].value, &flux_hour);	//hour of the day
		to_double( V["ambient"][0]["latitude"].value, &lat);
		to_double( V["ambient"][0]["longitude"].value, &lon);
		to_double( V["ambient"][0]["time_zone"].value, &tmz);

		DateTime DT;
		int doy = DT.GetDayOfYear(2011, int(flux_month), int(flux_day));
		
		//Instantiate the solpos object
		struct posdata SP, *pdat;
		pdat = &SP;	//point to structure for convenience
		S_init(pdat);		//Initialize the values

		//Calculate minutes/seconds
		double
			mins = 60.*(flux_hour - floor(flux_hour)),
			secs = 60.*(mins - floor(mins));
	
		pdat->latitude = float(lat);		//[deg] {float} North is positive
		pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
		pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
		pdat->year = 2011;		//[year] {int} 4-digit year
		pdat->month = int(flux_month);	//[mo] {int} (1-12)
		pdat->day = int(flux_day);		//[day] {int} Day of the month
		pdat->daynum = doy;	//[day] {int} Day of the year
		pdat->hour = int(flux_hour+.0001);		//[hr] {int} 0-23
		pdat->minute = int(mins);	//[min] {int} 0-59
		pdat->second = int(secs);	//[sec]	{int} 0-59
		pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


		long retcode = 0;		//Initialize with no errors
		retcode = S_solpos(pdat);	//Call the solar posotion algorithm
		S_decode(retcode, pdat);	//Check the return code

		az = SP.azim;
		zen = SP.zenetr;

		V["fluxsim"][0]["flux_solar_az"].value = to_string(az*d2r);
		V["fluxsim"][0]["flux_solar_el"].value = to_string(Pi/2. - zen*d2r);

	}
	//-- end flux simulation page --

	//end calculations
}

void interop::GenerateSimulationWeatherData(var_set &vset, int design_method, ArrayString &wf_entries){
	/* 
	Calculate and fill the weather data steps needed for simulation and the associated time step.

	The weather data is filled in the variable set vset["solarfield"][0]["sim_step_data"].value

	wf_entries consists of a list strings corresponding to each time step. 
	Each string is comma-separated and has the following entries:
	day, hour, month, dn, tdry, pres/1000., wspd
	*/
	
	string *tstepvar = &vset["solarfield"][0]["sim_time_step"].value;
	string *wdatvar = &vset["solarfield"][0]["sim_step_data"].value; 

	switch (design_method)
	{
	case LAYOUT_DETAIL::SUBSET_HOURS:

	{		//Subset of days/hours
		//Need to add this still
		throw spexception("Simulation with a user-specified list of days/hours is not currently supported. Please use another option.");
		*tstepvar = "0.";
		break;
	}
	case LAYOUT_DETAIL::SINGLE_POINT:
	{  //2) Single design point=1;
		*tstepvar = "0.";		
		*wdatvar = Ambient::getDefaultSimStep();
		break;
	}
	case LAYOUT_DETAIL::NO_FILTER:
	{	//3) Do not filter heliostats=0;
		wdatvar->clear();
		*tstepvar = "0";
		break;
	}
	case LAYOUT_DETAIL::FULL_ANNUAL:
	{	//4) Annual simulation=3;
		*wdatvar = vset["ambient"][0]["wf_data"].value;
		*tstepvar = "3600";
		break;
	}
	case LAYOUT_DETAIL::MAP_TO_ANNUAL:
	{  //Efficiency map + annual simulation
		*tstepvar = "3600";
		wdatvar->clear();
		
		vector<int> uday;
		vector<vector<double> > utime;
		double
			d2r = acos(-1.)/180.,
			lat = vset["ambient"][0]["latitude"].value_double()*d2r,
			lng = vset["ambient"][0]["longitude"].value_double()*d2r,
			tmz = vset["ambient"][0]["time_zone"].value_double(),
			dni_des = vset["solarfield"][0]["dni_des"].value_double();
		int nday = vset["solarfield"][0]["des_sim_ndays"].value_int();
		Ambient::calcSpacedDaysHours(lat, lng, tmz, nday, 1., utime, uday);

		int nflux_sim = 0;
		for(int i=0; i<(int)utime.size(); i++)
			nflux_sim += utime.at(i).size();
		
		DateTime dt;
		double month, dom, hoy, hod;
		for(int i=0; i<nday; i++){
			int doy = uday.at(i);	//because of the doy calcluation used before, this is actually [0..364]
			for(int j=0; j<(int)utime.at(i).size(); j++){
				hod = utime.at(i).at(j)+12.;
				hoy = double( doy ) * 24.;
				dt.hours_to_date( hoy, month, dom );	//midnight on the month/day
								
				char ts[150];
						sprintf(ts,"[P]%d,%f,%d,%f,%f,%f,%f,%f", int(dom), hod, int(month), dni_des, 25., 1., 1., 1.);
						wdatvar->append(ts);
			}
		}
		break;

	}
	case LAYOUT_DETAIL::LIMITED_ANNUAL:
	case LAYOUT_DETAIL::AVG_PROFILES:
	{	//5) Limited annual simulation=4 || Representative profiles=5
		*tstepvar = "0";  //calculate sun position at time specified
		wdatvar->clear();

		//Datetime object
		DateTime dt;
		double
			lat = vset["ambient"][0]["latitude"].value_double(),
			lng = vset["ambient"][0]["longitude"].value_double(),
			tmz = vset["ambient"][0]["time_zone"].value_double();

		int nday = vset["solarfield"][0]["des_sim_ndays"].value_int();
		int nskip = vset["solarfield"][0]["des_sim_nhours"].value_int();

		double delta_day = 365./float(nday);
		double doffset;
		if(nday%2 == 1){ doffset = 0.; }
		else{ doffset = delta_day/2.; }
		vector<int> simdays(nday,0);
		
		int dinit = 171-(int)Toolbox::round( ( floor(float(nday)/2.) - 1 )*delta_day - doffset );
		int dcalc;
		for(int i=0; i<nday; i++){ 
			dcalc = int( Toolbox::round(dinit + i*delta_day) );
			if(dcalc < 1){ dcalc += 365; }
			else if(dcalc > 365){ dcalc += -365; }
			simdays.at(i) = dcalc - 1;		//Day based on 0 index [0..364]
		}
		//Sort chronologically
		quicksort(simdays, 0, nday-1);

		//Calculate the month and day for each item
		double d2r = acos(-1.)/180.;
		vector<string> tsdat;
		for(int i=0; i<nday; i++){
			double month, dom, hoy;
			int doy = simdays.at(i);	//because of the doy calcluation used before, this is actually [0..364]
			hoy = double( doy ) * 24.;
			dt.hours_to_date( hoy, month, dom );	//midnight on the month/day

			//---Get some info on the weather for each day---
			
			//Calculate the start and end of daytime hours
			dt.SetHour(12);
			dt.SetDate(2011,(int)month,(int)dom);
			doy++;	//Now correct for the doy index
			dt.SetYearDay(doy);	
			double hrs[2];
			Ambient::calcDaytimeHours(hrs, lat*d2r, lng*d2r, tmz, dt);
					

			//Add up the total and average DNI's
			double dni, dni_tot = 0., dni_peak = 0.;
			double tdry, pres, wind;
			
			double hrmid = (hrs[0] + hrs[1])/2. + hoy;
			int nhrs = (int)(floor(hrs[1] - hrs[0]));
			
			//make sure the start and end hours are symmetric about solar noon
			double nmidspan = (double)nskip*floor(nhrs/(2.*(double)nskip));
			double hr_st = hrmid - nmidspan;
			double hr_end = hrmid + nmidspan;
				

			//Handle the "limited annual.." and "representative profile" options differently
			if(design_method == LAYOUT_DETAIL::LIMITED_ANNUAL){	//Limited annual simulation
				//preprocess the day's weather
				vector<double>
					all_time, all_dni, all_tdry, all_pres, all_wind, all_weights;
				double jd = hr_st;
				while(jd<hr_end+.001){	//include hr_end
					//index associated with time jd
					int jind = (int)floor(jd);	//the (j-1) originally may have been an error
					tsdat = split(wf_entries.at(jind), ",");

					to_double(tsdat.at(3), &dni);
					to_double(tsdat.at(4), &tdry);
					to_double(tsdat.at(5), &pres);
					to_double(tsdat.at(6), &wind);

					all_time.push_back(jd);
					all_dni.push_back(dni);
					all_tdry.push_back(tdry);
					all_pres.push_back(pres);
					all_wind.push_back(wind);

					//Calculate weighting factor for this hour
					double hod = fmod(jd,24.);
					double step_weight;
					if(jd==hr_st){
						step_weight = hod - hrs[0] + nskip/2.;
					}
					else if(jd + nskip < hr_end + 0.001){
						step_weight = nskip;
					}
					else{
						step_weight = hrs[1] - hod + nskip/2.;
					}
					step_weight *= Toolbox::round(delta_day);
					all_weights.push_back( step_weight );

					jd += (double)nskip;	
				}

				int ndatpt = (int)all_time.size();

				//weighting fractions for DNI
				double fthis = all_time.front() - floor(all_time.front());
				double fcomp = 1.-fthis;

				//for integration, which way should we go?
				int iind = fthis < 0.5 ? -1 : 1;
				
				for(int i=0; i<ndatpt; i++){

					//calculate the adjusted DNI based on the time surrounding the simulation position
					double dnimod, dnicomp;
					if(iind > 0)
						dnicomp = i < ndatpt - 1 ? all_dni.at(i+1) : 0.;
					else
						dnicomp = i > 0 ? all_dni.at(i-1) : 0.;

					dnimod = all_dni.at(i)*fthis + dnicomp * fcomp;

					double hod = fmod(all_time.at(i),24.);

					char ts[150];
					sprintf(ts,"[P]%d,%f,%d,%f,%f,%f,%f,%f", int(dom), hod, int(month), dnimod, all_tdry.at(i), all_pres.at(i),all_wind.at(i), all_weights.at(i) );
					wdatvar->append(ts);
				}

			}
			else	//Representative profile
			{
				//For each base day, loop through the days within the profile range. Average all values within the range.
				
				//Based on the simdays array, calculate the starting and ending day to include in the range
				int simprev;
				if(i==0) simprev = simdays.back();
				else simprev = simdays.at(i-1);
				int simnext;
				if(i==nday-1) simnext = simdays.at(0);
				else simnext = simdays.at(i+1);

				int dprev = 
					simprev > simdays.at(i) ? simdays.at(i)-(simprev-365) : simdays.at(i)-simprev;
				int dnext =
					simnext < simdays.at(i) ? simnext+365 - simdays.at(i) : simnext - simdays.at(i);

				int range_start = -dprev/2; //simdays.at(i)-dprev/2;
				int range_end = dnext/2; //simdays.at(i)+dnext/2;
				int range = range_end - range_start;

				//weighting fractions for DNI
				double fthis = hr_st - floor(hr_st);
				double fcomp = 1.-fthis;

				//for integration, which way should we go?
				int iind = fthis < 0.5 ? -1 : 1;

				int dayind=0;
				int nwf = wf_entries.size();
				double dnicomp;
				double jd=hr_st;
				while(jd < hr_end + 0.001){
					double tdry_per = 0., pres_per = 0., wind_per = 0., dni_per = 0.;
					for(int k=range_start; k<range_end; k++){
						int ind = (int)floor(jd)+k*24;
						if(ind < 0) ind += 8760;
						if(ind > 8759) ind += -8760;

						tsdat = split(wf_entries.at(ind), ",");
						to_double(tsdat.at(3), &dni);
						to_double(tsdat.at(4), &tdry);
						to_double(tsdat.at(5), &pres);
						to_double(tsdat.at(6), &wind);

						//get the complement dni data
						tsdat = split(wf_entries.at( fmin( fmax(ind+iind, 0), nwf-1) ), ",");
						to_double(tsdat.at(3), &dnicomp);


						//
						dni_per += dni * fthis + dnicomp * fcomp;
						tdry_per += tdry;
						pres_per += pres;
						wind_per += wind;

					}

					dni_per *= 1./(double)range;
					tdry_per *= 1./(double)range;
					pres_per *= 1./(double)range;
					wind_per *= 1./(double)range;
					//daily peak dni and total daily dni
					//dni_tot += dni_per/1000.;
					//if(dni_per > dni_peak) dni_peak = dni_per;

					//day, hour, month, dni, tdry, pres, wind
					double hod = fmod(jd,24.);

					//Calculate weighting factor for this hour
					double step_weight;
					if(jd==hr_st){
						step_weight = hod - hrs[0] + nskip/2.;
					}
					else if(jd + nskip < hr_end + 0.001){
						step_weight = nskip;
					}
					else{
						step_weight = hrs[1] - hod + nskip/2.;
					}
					step_weight *= (double)range;

					char ts[150];
					sprintf(ts, "[P]%d,%f,%d,%f,%f,%f,%f,%f", int(dom), hod, int(month), dni_per, tdry_per, pres_per, wind_per, step_weight);
					wdatvar->append(ts);

					dayind++;
					jd+=nskip;
				}
			}
		}
		break;
	}
	default:
		break;
	}
}

void interop::GenerateSimulationWeatherData(var_set &vset, int design_method, vector<string> &wf_entries)
{
	/* 
	OVERLOAD to support simple vector<string> type.
	
	Calculate and fill the weather data steps needed for simulation and the associated time step.

	The weather data is filled in the variable set vset["solarfield"][0]["sim_step_data"].value

	wf_entries consists of a list strings corresponding to each time step. 
	Each string is comma-separated and has the following entries:
	day, hour, month,  dni, tdry, pres, wspd
	1..,  0..,  1-12, W/m2,    C,  bar,  m/s
	*/

	//create an array string and call the main method
	ArrayString wfdat;
	for(int i=0; i<(int)wf_entries.size(); i++)
		wfdat.Add( wf_entries.at(i) );

	interop::GenerateSimulationWeatherData(vset, design_method, wfdat);


}

bool interop::parseRange(string &range, int &rangelow, int &rangehi, bool &include_low, bool &include_hi){
	//take range string of form:
	// {dlow,dhi} 
	// where {} can be replaced by ( ), [ ], ( ], [ )

	//parse the string
	vector<string> t1 = split(range, ",");
	if(t1.size()<2) return false;

	string lop, rop, ops, ls, rs;
	ls = t1.at(0);
	rs = t1.at(1);
	lop = ls.at(0);
	rop = rs.at(rs.size()-1);
	//Convert range values to integers
	to_integer(ls.erase(0,1), &rangelow);
	to_integer(rs.erase(rs.size()-1,1), &rangehi);

	ops = lop+rop;
	if(ops == " "){return false;}	//no info, don't check

	if(lop == "(") include_low = false;
	else include_low = true;
	if(rop == ")") include_hi = false;
	else include_hi = true;

	return true;

}

void interop::ticker_initialize(int indices[], int n){
	for(int i=0; i<n; i++)
		indices[i]=0;
	
}

bool interop::ticker_increment(int lengths[], int indices[], bool changed[], int n){
	/* 
	take an index array 'indices[len = n]' with maximum lengths 'lengths[len =n]' and increase
	the indices by '1'. The indices work like a scrolling counter. For example:
	if: lengths = {3,2,2}
	1	|	indices = {0,0,0}
	-> increment
	2	|	indices = {0,0,1}
	-> increment
	3	|	indices = {0,1,0}
	-> increment
	4	|	indices = {0,1,1}
	etc... 

	When initialized, indices are as shown in step 1.

	Returns false if the ticker has been exhausted
	*/

	//initialize 'changed' array
	for(int i=0; i<n; i++) changed[i] = false;

	//increment 
	bool inc_next=true;
	bool complete = false;
	for(int i=n-1; i>-1; i+=-1){
		if(inc_next){
			indices[i]++;
			changed[i] = true;

			//check for completion
			if(i==0)
				complete = indices[0]==lengths[0];
		}
		inc_next = indices[i] > lengths[i]-1;
		if(! inc_next) break;
		indices[i] = 0;
	}

	
	return complete;
}

//Simulation methods

void interop::AimpointUpdateHandler(SolarField &SF, var_set &vset){
	/* 
	Update the calculated aim points for each heliostat in the field depending on the 
	selection of the aiming method "aim_method". 

	Each method requires certain input parameters to be set, and this method acts as the
	intermediary in setting those relevant parameters.
	*/

	//Which type of simulation is this?
	int simtype;
	to_integer(vset["fluxsim"][0]["flux_model"].value, &simtype);	//0=Delsol, 1=Soltrace
	
	/*--- calculate aim points ---*/

	int aim_method;
	to_integer(vset["fluxsim"][0]["aim_method"].value, &aim_method);
	if(aim_method == 0){	//Simple aim points
		SF.calcAllAimPoints(0,(double*)NULL);
	}
	else if(aim_method == 1 || simtype==1){	//Sigma aim points=1, SolTrace can only handle this type right now.
		//Alternative aim points
		double sigma, args[2];
		to_double(vset["fluxsim"][0]["sigma_limit"].value, &sigma);
		args[0] = sigma;
		SF.calcAllAimPoints(aim_method,args,1);
	}
	else if(aim_method == 2){	//probability shift=2 
		double sigma, args[3];
		to_double(vset["fluxsim"][0]["sigma_limit"].value, &sigma);
		double dist;
		to_double(vset["fluxsim"][0]["flux_dist"].value, &dist);
		if(dist == 1.){
			//Normal dist, get the standard dev of the sampling distribution
			to_double(vset["fluxsim"][0]["norm_dist_sigma"].value, &args[2]);
		}
		args[0] = sigma;
		args[1] = dist;	//the distribution type
		SF.calcAllAimPoints(aim_method,args,3);
	}
	else if(aim_method == 3){ //Image size priority
		double sigma, args[2];	//args[0] = sigma, args[1] = reset flux grid flag
		to_double(vset["fluxsim"][0]["sigma_limit"].value, &sigma);
		args[0] = sigma;
		SF.calcAllAimPoints(aim_method, args, 1);
	}
	else if(aim_method == 4){ //Don't update the aim points
		//do nothing
	}

	else{ return; }
}

bool interop::PerformanceSimulationPrep(SolarField &SF, var_set &vset, Hvector &helios, int sim_method){
	/* 
	Call this method when setting up a performance simulation (i.e. a flux simulation).

	This method updates the Receiver flux map structure, the heliostat aim points, and the 
	sun position. Once all are updated, a single performance simulation is executed at the 
	specified sun position or hour/day combo (depending on user input).

	AFTER THIS METHOD:
	(A) In the GUI --- Call either HermiteFluxSimulationHandler() or SolTraceFluxSimulation()
		to do a full performance simulation for the system.
	-or-
	(B) Externally --- Call the SolarField::HermiteFluxSimulation() method to simulate performance.
	*/
	
	//Update the receiver surface flux densities
	int nx, ny;
	to_integer(vset["fluxsim"][0]["x_res"].value, &nx);
	to_integer(vset["fluxsim"][0]["y_res"].value, &ny);
	vector<Receiver*> *recs = SF.getReceivers();
	
	for(unsigned int i=0; i<recs->size(); i++){
		recs->at(i)->DefineReceiverGeometry(nx, ny);
	}

	//update clouds
	double ext[2];
	SF.getLandObject()->getExtents(ext, SF.getTowerHeight() );
	SF.getCloudObject()->Create(vset["fluxsim"][0], ext);
	//if(SF.getCloudObject()->isCloudy()){
	for(int i=0; i<(int)helios.size(); i++){
		double eta_cloud = SF.getCloudObject()->ShadowLoss( *helios.at(i)->getLocation() );
		helios.at(i)->setEfficiencyCloudiness( eta_cloud );
		//update overall efficiency
		helios.at(i)->calcTotalEfficiency();
	}
	//}
	/*--- calculate aim points ---*/


	//need to update the SF sun position before continuing
	if(vset["fluxsim"][0]["flux_time_type"].value == "0"){	//Sun position specified
		double az,el,d2r=acos(-1.)/180.;
		to_double(vset["fluxsim"][0]["flux_solar_az_in"].value, &az);
		to_double(vset["fluxsim"][0]["flux_solar_el_in"].value, &el);
		SF.getAmbientObject()->setSolarPosition(az*d2r,(90.-el)*d2r);
	}
	else{
		//day and time specified

		//Run a simulation for the specified conditions
		//<day of the month>, <hour of the day>, <month (1-12)>, <dni [W/m2]>,<amb. temperature [C]>, <atm. pressure [atm]>, <wind velocity [m/s]>
	
		int flux_day = vset["fluxsim"][0]["flux_day"].value_int();
		double flux_hour = vset["fluxsim"][0]["flux_hour"].value_double();
		int flux_month = vset["fluxsim"][0]["flux_month"].value_int();
		
		int doy = SF.getAmbientObject()->getDateTimeObj()->GetDayOfYear(2011,flux_month, flux_day);
		SF.getAmbientObject()->setDateTime(flux_hour, doy);
		SF.getAmbientObject()->calcSunPosition();
				
	}

	double dni;
	to_double(vset["solarfield"][0]["dni_des"].value, &dni);
	double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
	
	/* 
	If the performance simulation requires detailed flux information, do an initial calculation with simple aimpoints.
	This allows for accurate (optimistic) power delivery values from each heliostat.
	*/
	if( vset["fluxsim"][0]["aim_method"].value_int() == Flux::AIM_STRATEGY::IMAGE_SIZE ){
		//update aim points to simple position
		SF.calcAllAimPoints(0,(double*)NULL);
		//Simulate with simple aimpoints to update heliostat efficiency
		SF.Simulate(args, 4);
	}

	//if we're using a SolTrace simulation, the options are simple aim points or to use 
	//the previously calculated values using Delsol
	if(! (sim_method==1 && SF.getAimpointStatus()) ){
		//calculate aim points according to specified options
		AimpointUpdateHandler(SF, vset);
	}
	//After updating the aim points, run a performance simulation to determine actual heliostat efficiencies
	SF.Simulate(args, 4);

	return !SF.ErrCheck();
			
}

#ifdef _USE_SOLTRACE
bool interop::SolTraceFluxSimulation_ST(st_context_t cxt, int seed, ST_System &ST,
										int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
										void *par)
{
	/* 
	This method requires that a SolTrace context "st_context_t" has already been created and passed to 
	the method. 
	Create by calling:
	st_context_t cxt = st_create_context();

	This method sets up and executes a single-threaded SolTrace simulation. 
	Returns status (error = false, no error = true)

	Passes an optional pointer to a callback function that updates the GUI.
	*/	
	int minrays = ST.sim_raycount;
	int maxrays = ST.sim_raymax;
	
	//simulate, setting the UI callback and a pointer to the UI class
	
	st_sim_params( cxt, minrays, maxrays );
	return st_sim_run(cxt, seed, callback, par) != -1;
}


bool interop::SolTraceFluxSimulation_ST(st_context_t cxt, SolarField &SF, var_set &vset, Hvector &helios,
							   int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
							   void *par)
{
	//Overload to be called when maintaining ST_System is not important
	
	ST_System STSim;
	STSim.CreateSTSystem(vset, SF, helios);
	ST_System::LoadIntoContext(&STSim, cxt);
	int seed = SF.getFluxObject()->getRandomObject()->integer();
	return SolTraceFluxSimulation_ST(cxt, seed, STSim, callback, par);
}
#endif

void interop::UpdateMapLayoutData(var_set &vset, Hvector *heliostats){
//Fill in the data
	int npos = heliostats->size();
	Heliostat *H;

	string *var = &vset["solarfield"][0]["layout_data"].value;
	var->clear();
	string sdat; //, sdat2, sdat3, sdat4;
	

	for(int i=0; i<npos; i++){
		/* Fill in the data for each heliostat in the template */


		H = heliostats->at(i);	//shorthand the pointer
		if(! H->getInLayout()) continue;

		Point *loc = H->getLocation();
		Vect *cant = H->getCantVector();
		Point *aim = H->getAimPoint();

		//Save the layout to the variable maps
		//Take special care for user-specified values vs. program calculated values.
		char tchar1[300];
		if(H->IsUserFocus()){ sprintf(tchar1, "%f,%f", H->getFocalX(), H->getFocalY()); }
		else{ sprintf(tchar1, "NULL,NULL"); }
		//sdat2 = string(tchar1);

		char tchar2[300];
		if(H->IsUserCant()){ sprintf(tchar2, "%f,%f,%f", cant->i, cant->j, cant->k); }
		else{ sprintf(tchar2, "NULL,NULL,NULL"); }
		//sdat3 = string(tchar2);

		char tchar3[300];
		sprintf(tchar3, "%f,%f,%f", aim->x, aim->y, aim->z); 
		//sdat4 = string(tchar3);

		char tchar4[300];
		sprintf(tchar4, "%d,%f,%f,%f,%s,%s,%s\n",H->getType(), loc->x, loc->y, loc->z, tchar1, tchar2, tchar3);
		sdat = string(tchar4);
		var->append(sdat);

	}

}

//-----


void stat_object::initialize(){ //int size){
	min = 9.e99; 
	max = -9.e99;
	sum = 0.;
	stdev = 0.;
	ave = 0.;

}

void stat_object::set(double _min, double _max, double _ave, double _stdev, double _sum){
	min = _min;
	max = _max;
	ave = _ave;
	stdev = _stdev;
	sum = _sum;
}

//-----------------------
//cost
//cost_categories::cost_categories(){
//	reset();
//}
//
//void cost_categories::reset()
//{
//	//initialize
//	c_heliostat = 0.;
//	c_receiver = 0.;
//	c_tower = 0.;
//	c_tes = 0.;
//	c_pb = 0.;
//	c_other = 0.;
//	c_total = 0.;
//}
//
//void cost_categories::calculateTotalCost(){
//			
//	c_total = c_heliostat + c_receiver + c_tower + c_tes + c_pb + c_other;
//}

//-----------------------
sim_result::sim_result(){
	initialize();
}

void sim_result::initialize(){
	total_heliostat_area = 0.;
	total_receiver_area = 0.;
	total_land_area = 0.;
	power_on_field = 0.;
	power_absorbed = 0.;
	power_to_cycle = 0.;
	power_gross = 0.;
	power_net = 0.;
	num_heliostats_used = 0;
	num_heliostats_avail = 0;
	_q_coe = 0.;

	eff_total_heliostat.initialize();
	eff_total_sf.initialize();
	eff_cosine.initialize();
	eff_attenuation.initialize();
	eff_blocking.initialize();
	eff_shading.initialize();
	eff_reflect.initialize();
	eff_intercept.initialize();
	eff_absorption.initialize();
	flux_density.initialize();

	flux_surfaces.clear();
	data_by_helio.clear();
}

void sim_result::add_heliostat(Heliostat &H){
	H.getEfficiencyObject()->rec_absorptance = H.getWhichReceiver()->getAbsorptance();
	data_by_helio[ H.getId() ] = *H.getEfficiencyObject();
	num_heliostats_used++;
	total_heliostat_area += H.getArea();
	_q_coe += H.getPowerValue();
}

void sim_result::process_field_stats(){
	//Calculate statistics for all of the heliostats
	int nm = (data_by_helio.begin()->second).n_metric;
	double 
		*sums = new double[nm],
		*aves = new double[nm],
		*stdevs = new double[nm],
		*mins = new double[nm],
		*maxs = new double[nm];
	
	double *aves2 = new double[nm];		//Temporary array for calculating variance

	for(int i=0; i<nm; i++){ 
		sums[i] = 0.;
		stdevs[i] = 0.;
		mins[i] = 9.e9;
		maxs[i] = -9.e9;
		aves[i] = 0.;
		aves2[i] = 0.;
	}

	//Calculate metrics
	int n=0;
	double delta;
	for(unordered_map<int, helio_perf_data>::iterator it = data_by_helio.begin(); it != data_by_helio.end(); it++){
		n++;
		for(int j=0; j<nm; j++){
			double v = it->second.getDataByIndex(j);
			sums[j] += v;
			if(v > maxs[j]) maxs[j] = v;
			if(v < mins[j]) mins[j] = v;	

			//Average and variance estimates using Knuth's method
			delta = v - aves[j];
			aves[j] += delta / (double)n;
			aves2[j] += delta*(v-aves[j]);
		}
	}
	//Calculate final standard deviation
	for(int j=0; j<nm; j++)
		stdevs[j] = sqrt(aves2[j]/(double)(n-1));

	delete [] aves2;

	//Assign the named variables
	eff_total_heliostat.set( 
		mins[ helio_perf_data::ETA_TOT ], 
		maxs[ helio_perf_data::ETA_TOT ], 
		aves[ helio_perf_data::ETA_TOT ], 
		stdevs[ helio_perf_data::ETA_TOT ], 
		sums[ helio_perf_data::ETA_TOT ]);
	eff_cosine.set(
		mins[ helio_perf_data::ETA_COS ], 
		maxs[ helio_perf_data::ETA_COS ], 
		aves[ helio_perf_data::ETA_COS ], 
		stdevs[ helio_perf_data::ETA_COS ], 
		sums[ helio_perf_data::ETA_COS ]);
	eff_attenuation.set(
		mins[ helio_perf_data::ETA_ATT ], 
		maxs[ helio_perf_data::ETA_ATT ], 
		aves[ helio_perf_data::ETA_ATT ], 
		stdevs[ helio_perf_data::ETA_ATT ], 
		sums[ helio_perf_data::ETA_ATT ]);
	eff_blocking.set(
		mins[ helio_perf_data::ETA_BLOCK ], 
		maxs[ helio_perf_data::ETA_BLOCK ], 
		aves[ helio_perf_data::ETA_BLOCK ], 
		stdevs[ helio_perf_data::ETA_BLOCK ], 
		sums[ helio_perf_data::ETA_BLOCK ]);
	eff_shading.set(
		mins[ helio_perf_data::ETA_SHADOW ], 
		maxs[ helio_perf_data::ETA_SHADOW ], 
		aves[ helio_perf_data::ETA_SHADOW ], 
		stdevs[ helio_perf_data::ETA_SHADOW ], 
		sums[ helio_perf_data::ETA_SHADOW ]);
	eff_intercept.set(
		mins[ helio_perf_data::ETA_INT ], 
		maxs[ helio_perf_data::ETA_INT ], 
		aves[ helio_perf_data::ETA_INT ], 
		stdevs[ helio_perf_data::ETA_INT ], 
		sums[ helio_perf_data::ETA_INT ]);
	eff_absorption.set(
		mins[ helio_perf_data::REC_ABSORPTANCE ], 
		maxs[ helio_perf_data::REC_ABSORPTANCE ], 
		aves[ helio_perf_data::REC_ABSORPTANCE ], 
		stdevs[ helio_perf_data::REC_ABSORPTANCE ], 
		sums[ helio_perf_data::REC_ABSORPTANCE ]);
	eff_cloud.set(
		mins[ helio_perf_data::ETA_CLOUD ], 
		maxs[ helio_perf_data::ETA_CLOUD ], 
		aves[ helio_perf_data::ETA_CLOUD ], 
		stdevs[ helio_perf_data::ETA_CLOUD ], 
		sums[ helio_perf_data::ETA_CLOUD ]);
	eff_reflect.set(
		mins[ helio_perf_data::REFLECTIVITY ]*mins[ helio_perf_data::SOILING ], 
		maxs[ helio_perf_data::REFLECTIVITY ]*maxs[ helio_perf_data::SOILING ], 
		aves[ helio_perf_data::REFLECTIVITY ]*aves[ helio_perf_data::SOILING ], 
		stdevs[ helio_perf_data::REFLECTIVITY ]*stdevs[ helio_perf_data::SOILING ], 
		sums[ helio_perf_data::REFLECTIVITY ]*sums[ helio_perf_data::SOILING ]
	);
	eff_total_sf.set(
		mins[ helio_perf_data::ETA_TOT ]*mins[ helio_perf_data::REC_ABSORPTANCE ], 
		maxs[ helio_perf_data::ETA_TOT ]*maxs[ helio_perf_data::REC_ABSORPTANCE ], 
		aves[ helio_perf_data::ETA_TOT ]*aves[ helio_perf_data::REC_ABSORPTANCE ], 
		stdevs[ helio_perf_data::ETA_TOT ]*stdevs[ helio_perf_data::REC_ABSORPTANCE ], 
		sums[ helio_perf_data::ETA_TOT ]*sums[ helio_perf_data::REC_ABSORPTANCE ]
	);
	
	delete [] sums;
	delete [] aves;
	delete [] stdevs;
	delete [] mins; 
	delete [] maxs;
}

void sim_result::process_flux_stats(SolarField &SF){
	//Determine the flux info
	double atot = SF.getReceiverTotalArea();
	double fave=0., fave2=0., fmax = -9.e9, fmin = 9.e9;
	int nf = 0;
	vector<Receiver*> *recs = SF.getReceivers();
	for( int i=0; i<(int)recs->size(); i++){
		FluxSurfaces *fs = recs->at(i)->getFluxSurfaces();
		for(int j=0; j<(int)fs->size(); j++){
			FluxGrid *fg = fs->at(j).getFluxMap();
			//double ascale = fs->at(j).getSurfaceArea()/atot;
			int 
				nx = fs->at(j).getFluxNX(),
				ny = fs->at(j).getFluxNY();
			double v, delta;
			for(int k=0; k<nx; k++){
				for(int l=0; l<ny; l++){
					v = fg->at(k).at(l).flux;
						
					if(v > fmax) fmax = v;
					if(v < fmin) fmin = v;

					//Estimate variance - use Knuth's one-pass method (http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance) 
					nf++;
					delta = v - fave;
					fave += delta/(double)nf;
					fave2 += delta*(v-fave);

				}
			}
		}
	}
	//Calculate final variance value
	flux_density.stdev = sqrt(fave2/(double)(nf-1));
	flux_density.max = fmax;
	flux_density.min = fmin;
	flux_density.ave = fave;
}

void sim_result::process_analytical_simulation(SolarField &SF, int nsim_type, Hvector &helios){
	is_soltrace = false;
	sim_type = nsim_type;

	switch (sim_type)
	{
	case sim_result::SIM_TYPE::PARAMETRIC:
	case sim_result::SIM_TYPE::OPTIMIZATION:
	case sim_result::SIM_TYPE::LAYOUT:
		//process only the ranking metric for each heliostat
		initialize();
		for(unsigned int i=0; i<helios.size(); i++)
			add_heliostat(*helios.at(i));
		
		total_receiver_area = SF.getReceiverTotalArea();
		dni =  SF.getDesignPointDNI()/1000.;
		power_on_field = total_heliostat_area * dni;	//[kW]
		power_absorbed = power_on_field * eff_total_sf.ave;
		solar_az = SF.getAmbientObject()->getSolarAzimuth();
		solar_zen = SF.getAmbientObject()->getSolarZenith();


		break;
	case sim_result::SIM_TYPE::FLUX_SIMULATION:

		initialize();
		for(unsigned int i=0; i<helios.size(); i++)
			add_heliostat(*helios.at(i));
		process_field_stats();
		total_receiver_area = SF.getReceiverTotalArea();
		dni =  SF.getDesignPointDNI()/1000.;
		power_on_field = total_heliostat_area * dni;	//[kW]
		power_absorbed = power_on_field * eff_total_sf.ave;
		solar_az = SF.getAmbientObject()->getSolarAzimuth();
		solar_zen = SF.getAmbientObject()->getSolarZenith();

		SF.getFinancialObject()->calcPlantCapitalCost(SF);	//Always update the plant cost

		total_installed_cost = SF.getFinancialObject()->getTotalInstalledCost();
		coe_metric = total_installed_cost/_q_coe;
		
		process_flux_stats(SF);

		break;
	default:
		break;
	}

}

void sim_result::process_analytical_simulation(SolarField &SF, int sim_type){  /*0=Layout, 1=Optimize, 2=Flux sim, 3=Parametric */
	process_analytical_simulation(SF, sim_type, *SF.getHeliostats());
};

void sim_result::process_raytrace_simulation(SolarField &SF, int nsim_type, Hvector &helios, double qray, int *emap, int *smap, int *rnum, int ntot, double *boxinfo){
	

	is_soltrace = true;
	/* sim_type: 2=flux simulation, 3=parametric */
	initialize();
	sim_type = nsim_type;
	if(sim_type == 2){

		num_heliostats_used = helios.size();
		for(int i=0; i<num_heliostats_used; i++){
			total_heliostat_area += helios.at(i)->getArea();
		}
		double dni = SF.getDesignPointDNI()/1000.;


		//Process the ray data
		int st, st0=0, ray, ray0=0, el, el0=0;
		int nhin=0, nhout=0, nhblock=0, nhabs=0, nrin=0, nrspill=0, nrabs=0;

		for(int i=0; i<ntot; i++){ //for each ray hit
			st = smap[i];	//Stage
			ray = rnum[i];	//Ray number
			el = emap[i];	//Element
			
			//Check first to see if the ray number has changed without logging data
			if((ray != ray0) && (ray0 != 0)){
				if( st0 == 1){ //Heliostat stage
					//Ray was successfully reflected out of the field
					nhin++;
					nhout++;
				}
				else{ //Receiver stage
					//Ray was lost through reflection from the receiver
					nrin++;
				}
				ray0 = 0;
				st0 = 0;
				el0 = 0;
			}

			if(el < 0){
				//Ray was absorbed 
				if(st == 1){ //Heliostat
					nhin++;
					if( ray == ray0 ) //Multiple intersections within heliostat field, meaning blocking occurred
						nhblock++;
					else
						nhabs++; //Single intersection -- reflectivity loss
				}
				else{  //Receiver
					nrin++;
					nrabs++;
				}
				el0 = 0;
				ray0 = 0;
				st0 = 0;
			}
			else if( el == 0 ){ //Element map is 0 - a ray missed the receiver
				nrspill++;

				el0 = 0;
				ray0 = 0;
				st0 = 0;
			}
			else{
				st0 = st;
				ray0 = ray;
				el0 = el;
			}
		}

		int nsunrays = (int)boxinfo[4];
		double Abox = (boxinfo[0] - boxinfo[1])*(boxinfo[2] - boxinfo[3]);

		power_on_field = total_heliostat_area *dni;	//[kW]
		power_absorbed = qray * nrabs;
		eff_total_sf.ave = power_absorbed / power_on_field;
		eff_cosine.ave = (double)nhin/(double)nsunrays*Abox/total_heliostat_area;
		eff_blocking.ave = 1.-(double)nhblock/(double)(nhin-nhabs);
		eff_attenuation.ave = 1.;	//Not currently accounted for
		eff_reflect.ave = (double)(nhin - nhabs)/(double)nhin;
		eff_intercept.ave = (double)nrin/(double)nhout;
		eff_absorption.ave = (double)nrabs/(double)nrin;
		eff_total_heliostat.ave = (double)nrabs/(double)nhin;

		total_receiver_area = SF.getReceiverTotalArea();
		solar_az = SF.getAmbientObject()->getSolarAzimuth();
		solar_zen = SF.getAmbientObject()->getSolarZenith();

		SF.getFinancialObject()->calcPlantCapitalCost(SF);	//Always update the plant cost

		total_installed_cost = SF.getFinancialObject()->getTotalInstalledCost();
		coe_metric = total_installed_cost/power_absorbed;

		process_flux_stats(SF);

	}
	else{

	}


}

void sim_result::process_flux(SolarField *SF, bool normalize){
	flux_surfaces.clear();
	receiver_names.clear();
	int nr = SF->getReceivers()->size();
	Receiver *rec;
	for(int i=0; i<nr; i++){
		rec = SF->getReceivers()->at(i);
		if(! rec->isReceiverEnabled() ) continue;
		flux_surfaces.push_back( *rec->getFluxSurfaces() );
		if(normalize){
			for(unsigned int j=0; j<rec->getFluxSurfaces()->size(); j++){
				flux_surfaces.back().at(j).Normalize();
			}
		}
		receiver_names.push_back( *SF->getReceivers()->at(i)->getReceiverName() );
	}
	
}

//------parametric------------------------
parametric::parametric(){wf_are_set = false;}
int parametric::size(){ return (int)variables.size(); }
void parametric::clear(){ variables.clear(); current_varpaths.Clear();}
par_variable &parametric::at(int index){ return variables.at(index); }
par_variable &parametric::operator[](int index){ return variables.at(index); }
par_variable &parametric::back(){ return variables.back(); }

void parametric::remove(int index){ 
	variables.erase(variables.begin()+index); 
	current_varpaths.erase(current_varpaths.begin()+index); 
}

void parametric::SetWeatherFileList(ArrayString &list){
	weather_files.clear();
	for(int i=0; i<list.size(); i++){
		weather_files.push_back(list[i]);
	}
	wf_are_set = true;
}

void parametric::addVar(var_data *var){
	/* 
	Add a new variable to the parametric analysis. If the variable already exists, overwrite it.
	*/


	//check to see whether this variable already exists. If so, overwrite it
	int curind = Index(var->varpath);
	par_variable *vback;
	if(curind > 0){
		variables.erase(variables.begin()+curind);
		variables.insert(variables.begin()+curind, par_variable());
		vback = &variables.at(curind);
	}
	else
	{
		current_varpaths.Add(var->varpath);
		variables.push_back(par_variable());
		vback = &variables.back();
	}
	
	vback->varname = var->varpath;
	vback->display_text = split(var->varpath, ".").at(0) + ": "+var->short_desc;
	vback->units = var->units;
	vback->selections.clear();
	
	if(var->varpath == "ambient.0.weather_file"){
		if(! wf_are_set) return;
		vback->selections.Add(var->value);
		vback->data_type = "location";
		//Load the possible weather files
		vback->choices.clear();
		for(int i=0; i<weather_files.size(); i++){
			vback->choices.push_back(weather_files[i]);
		}
	}
	else if(var->ctype == "combo"){
		vback->selections.Add( var->index_map[var->cselect] );
		vback->data_type = "combo";
		vback->choices.clear();
		for(unordered_map<int,string>::iterator it=var->index_map.begin(); it != var->index_map.end(); it++){
			vback->choices.push_back( it->second );
		}
	}
	else if(var->ctype == "checkbox"){
		vback->selections.Add(var->value);
		vback->data_type = "checkbox";
		vback->choices.clear();
		vback->choices.push_back("true");
		vback->choices.push_back("false");
	}
	else if(var->ctype == "bool"){
		vback->selections.Add(var->value);
		vback->data_type = "bool";
		vback->choices.clear();
		vback->choices.push_back("true");
		vback->choices.push_back("false");
	}
	else if(var->ctype == "int"){
		vback->selections.Add(var->value);
		vback->data_type = "int";
		int imin, imax;
		bool withlow, withhi;
		interop::parseRange(var->range, imin, imax, withlow, withhi);
		for(int i=(withlow?imin:imin+1); i<(withhi?imax:imax-1); i++){
			vback->choices.push_back( to_string(i) );
		}

	}

	else{	//doubles
		vback->selections.Add(var->value);
		vback->data_type = var->dattype;
	}
}

int parametric::Index(string pathname){
	return current_varpaths.Index(pathname);
}

//----- user par table -------------------
ArrayString &simulation_table::operator[](const string &varname){return data[varname];}
ArrayString &simulation_table::at(const string &varname){return data[varname];}
size_t simulation_table::nvar(){return data.size();}
size_t simulation_table::nsim(){return data.size() > 0 ? data.begin()->second.size() : 0;}

void simulation_table::ClearAll(){
	for(unordered_map<string, ArrayString>::iterator it=data.begin(); it != data.end(); it++)
		it->second.Clear();
	data.clear();
}

void simulation_table::getKeys(ArrayString &keys){
	keys.clear();

	for(unordered_map<string, ArrayString>::iterator it = data.begin(); it != data.end(); it++)
		keys.push_back(it->first);
	
}
