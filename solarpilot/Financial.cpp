#include "Financial.h"
#include "math.h"
#include <vector>
#include "SolarField.h"
using namespace std;

//--accessors

matrix_t<int> *Financial::getHourlyTODSchedule(){return &_schedule_array;} //8760 TOD matrix
double Financial::getTowerCost(){return _tower_cost;}
double Financial::getReceiverCost(){return _rec_cost;}
double Financial::getSiteCost(){return _site_cost;}
double Financial::getHeliostatCost(){return _heliostat_cost;}
double Financial::getWiringCost(){return _wiring_cost;}
double Financial::getPlantCost(){return _plant_cost;}
double Financial::getTESCost(){return _tes_cost;}
double Financial::getLandCost(){return _land_cost;}
double Financial::getFixedCost(){return _fixed_cost;}
double Financial::getContingencyCost(){return _contingency_cost;}
double Financial::getSalesTaxCost(){return _sales_tax_cost;}
double Financial::getTotalDirectCost(){return _total_direct_cost;}
double Financial::getTotalIndirectCost(){return _total_indirect_cost;}
double Financial::getTotalInstalledCost(){return _total_installed_cost;}
double Financial::getCostPerCapacity(){return _cost_per_capacity;}
	
	
bool Financial::isPaymentFactors(){return _is_pmt_factors;}
void Financial::isPaymentFactors(bool value){_is_pmt_factors = value;}

//
void Financial::Create(var_map &V){
	setVar("tower_fixed_cost", _tower_fixed_cost, V, 1927000., "[0,9e9]");		//Fixed tower cost - used as the basis for scaling tower cost as a function of height
	setVar("tower_exp", _tower_exp, V, 0.0113, "[-9e9,9e9]");		//Exponent in the equation (total cost) = (fixed cost) * exp( X * (tower height) )
	setVar("tower_cost", _tower_cost, V, 14736146.);		//Tower cost
	setVar("rec_ref_cost", _rec_ref_cost, V, 126200000., "[-9e9,9e9]");		//Cost of the receiver at the sizing indicated by the reference receiver area
	setVar("rec_ref_area", _rec_ref_area, V, 1571.);		//Receiver surface area corresponding to the receiver reference cost
	setVar("rec_cost_exp", _rec_cost_exp, V, 0.7, "[-9e9,9e9]");		//Exponent in the equation (total cost) = (ref. cost) * ( (area) / (ref. area) ) ^ X
	setVar("rec_cost", _rec_cost, V, 84944751.6);		//Receiver cost
	setVar("site_spec_cost", _site_spec_cost, V, 20., "[-9e9,9e9]");		//Cost per square meter of heliostat aperture area of site improvements
	setVar("site_cost", _site_cost, V, 0.);		//Site improvements cost
	setVar("heliostat_spec_cost", _heliostat_spec_cost, V, 180., "[-9e9,9e9]");		//Cost per square meter of heliostat aperture area of the heliostat field
	setVar("heliostat_cost", _heliostat_cost, V, 0.);		//Heliostat field cost
	setVar("wiring_cost", _wiring_cost, V, 0.);		//Wiring cost
	setVar("wiring_user_spec", _wiring_user_spec, V, 9., "[-9e9,9e9]");		//Cost of wiring per square meter of heliostat aperture area
	setVar("plant_cost", _plant_cost, V, 0.);		//Cost of the power block and balance of plant equipment
	setVar("plant_spec_cost", _plant_spec_cost, V, 1200.);		//Cost of the power block and balance of plant equipment per kilowatt (electric) gross design power
	setVar("tes_cost", _tes_cost, V, 0.);		//Thermal storage cost
	setVar("tes_spec_cost", _tes_spec_cost, V, 27.);		//Cost of thermal storage per kilowatt hour (thermal) capacity
	setVar("fixed_cost", _fixed_cost, V, 0.);		//Cost that does not scale with any plant parameter
	setVar("total_direct_cost", _total_direct_cost, V, 0.);		//Sum of all direct costs
	setVar("land_cost", _land_cost, V, 0.);		//Land cost
	setVar("land_spec_cost", _land_spec_cost, V, 10000.);		//Cost of land per acre including the footprint of the land occupied by the entire plant.
	setVar("contingency_rate", _contingency_rate, V, 7.);		//Fraction of the direct capital costs added to account for contingency
	setVar("contingency_cost", _contingency_cost, V, 0.);		//Contingency cost
	setVar("sales_tax_rate", _sales_tax_rate, V, 5.);		//Sales tax rate applid to the total direct capital cost
	setVar("sales_tax_frac", _sales_tax_frac, V, 80.);		//Fraction of the direct capital costs for which sales tax applies
	setVar("sales_tax_cost", _sales_tax_cost, V, 0.);		//Sales tax cost
	setVar("total_indirect_cost", _total_indirect_cost, V, 0.);		//Sum of all indirect costs
	setVar("total_installed_cost", _total_installed_cost, V, 0.);		//Sum of direct and indirect costs
	setVar("cost_per_capacity", _cost_per_capacity, V, 0.);		//Estimated capital cost per capacity (net)
	setVar("weekday_sched", _weekday_sched, V, "666666554444444444444555666666554444444444444555666666554444444444444555666666554444444444444555666666554444444444444555333333332222111111222333333333332222111111222333333333332222111111222333333333332222111111222333666666554444444444444555666666554444444444444555666666554444444444444555");		//Weekday dispatch period schedule
	setVar("weekend_sched", _weekend_sched, V, "666666555555555555555555666666555555555555555555666666555555555555555555666666555555555555555555666666555555555555555555333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333666666555555555555555555666666555555555555555555666666555555555555555555");		//Weekend dispatch period schedule
	setVar("is_pmt_factors", _is_pmt_factors, V, false);		//Enable or disable the use of weighting factors in determining field layout
	setVar("pmt_factor_1", _pmt_factor_1, V, 2.064);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_2", _pmt_factor_2, V, 1.2);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_3", _pmt_factor_3, V, 1.);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_4", _pmt_factor_4, V, 1.1);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_5", _pmt_factor_5, V, 0.8);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_6", _pmt_factor_6, V, 0.7);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_7", _pmt_factor_7, V, 1.);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_8", _pmt_factor_8, V, 1.);		//Relative value of electricity produced during this period compared to the average
	setVar("pmt_factor_9", _pmt_factor_9, V, 1.);		//Relative value of electricity produced during this period compared to the average

	//Assign the hourly schedules
	CreateHourlyTODSchedule(_weekday_sched, _weekend_sched, _schedule_array);

}

void Financial::Clean(){
	/* Clean key variables that shouldn't be available when a new solar field is created */
	_weekday_sched = "";
	_weekend_sched = "";
	_schedule_array.clear();
	for(int i=0; i<9; i++) _all_pmt_factors[i] = 1.;
	
}

void Financial::CreateHourlyTODSchedule(string &wd_schedule, string &we_schedule, matrix_t<int> &hr_array){
	/* 
	Take a schedule (12x24 = 288) of the TOD factors in string form and convert them into 
	an 8760 schedule of integers indicating the TOD factor for each hour of the year.

	Assume the year starts on a Sunday

	*/
	int nwd = wd_schedule.size();
	int nwe = we_schedule.size();

	if(nwd != 288 || nwe != 288) return;

	int monthlength[] = {31,28,31,30,31,30,31,31,30,31,30,31};

	hr_array.resize(8760);
	int h=0, tod;
	int dow = 6;	//M=0, T=1; W=2; Th=3; Fr=4, Sa=5; Su=6. Start on a Sunday
	string ss;
	for(int i=0; i<12; i++){
		for(int j=0; j<monthlength[i]; j++){
			for(int k=0; k<24; k++){
				ss = dow<5 ? wd_schedule.at(i*24+k) : we_schedule.at(i*24+k);
				to_integer(ss, &tod);
				hr_array[h] = tod;
				h++;
			}
			dow==6 ? dow = 0 : dow++ ;
		}
	}

}

double *Financial::getPaymentFactors(){
	/* 
	Collect all of the payment factors and return 
	Data is [9] deep
	*/

	_all_pmt_factors[0] = _pmt_factor_1;
	_all_pmt_factors[1] = _pmt_factor_2;
	_all_pmt_factors[2] = _pmt_factor_3;
	_all_pmt_factors[3] = _pmt_factor_4;
	_all_pmt_factors[4] = _pmt_factor_5;
	_all_pmt_factors[5] = _pmt_factor_6;
	_all_pmt_factors[6] = _pmt_factor_7;
	_all_pmt_factors[7] = _pmt_factor_8;
	_all_pmt_factors[8] = _pmt_factor_9;
	return _all_pmt_factors;

}

void Financial::calcPlantCapitalCost(SolarField &SF){
	
	_tower_cost = _tower_fixed_cost * exp(SF.getTowerHeight() * _tower_exp );
	
	_rec_cost = _rec_ref_cost * pow( SF.getReceiverTotalArea() / _rec_ref_area, _rec_cost_exp );
	
	_plant_cost = _plant_spec_cost * SF.getPlantObject()->getPlantGrossPower() * 1000.;
	
	_tes_cost = SF.getPlantObject()->getTESThermalCapacity() * 1000. * _tes_spec_cost;
	
	double Asf = SF.getHeliostatArea();
	
	_site_cost = _site_spec_cost * Asf;
	_heliostat_cost = _heliostat_spec_cost * Asf;
	_wiring_cost = _wiring_user_spec * Asf;

	_total_direct_cost = _tower_cost + _rec_cost + _site_cost + _heliostat_cost + _wiring_cost + _plant_cost + _tes_cost + _fixed_cost;

	_contingency_cost = _contingency_rate/100. * _total_direct_cost;

	_total_direct_cost += _contingency_cost;

	double Aland = SF.calcLandArea();
	if(Aland < 0. || Aland > 9.e9) Aland = 0.;
	_land_cost = Aland * _land_spec_cost / 4046.86;		//[m2 -> acre]

	_sales_tax_cost = _sales_tax_rate * _sales_tax_frac * _total_direct_cost / 1.e4;

	_total_indirect_cost = _sales_tax_cost + _land_cost;

	_total_installed_cost = _total_direct_cost + _total_indirect_cost;

	_cost_per_capacity = _total_installed_cost / SF.getPlantObject()->getPlantNetPower() / 1000.;

}

void Financial::calcSimpleCOE(double *enet, int nval){
	/* 
	Calculate the cost of energy on a simple capital cost to energy production ratio.
	This number is not representative of an actual LCOE.
	
	enet :: double[nval]
	*/
	if(nval > (int)_schedule_array.ncells()) return;

	double
		prod = 0.,
		prod_w = 0.;

	for(int i=0; i<nval; i++){
		double e = enet[i];
		prod_w += _schedule_array[i] * e;
		prod += e;
	}


	_simple_coe = _total_installed_cost/prod;
	_weighted_coe = _total_installed_cost/prod_w;

}
