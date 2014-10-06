#ifndef _FINANCIAL_H_
#define _FINANCIAL_H_ 1
#include "math.h"
#include <vector>
#include "mod_base.h"

using namespace std;
class SolarField;

class Financial : public mod_base
 {
   
	//Variable definitions
	double
		_tower_fixed_cost,		//Fixed tower cost - used as the basis for scaling tower cost as a function of height
		_tower_exp, 		//Exponent in the equation (total cost) = (fixed cost) * exp( X * (tower height) )
		_tower_cost,		//Tower cost
		_rec_ref_cost,		//Cost of the receiver at the sizing indicated by the reference receiver area
		_rec_ref_area,		//Receiver surface area corresponding to the receiver reference cost
		_rec_cost_exp,		//Exponent in the equation (total cost) = (ref. cost) * ( (area) / (ref. area) ) ^ X
		_rec_cost,			//Receiver cost
		_site_spec_cost,	//Cost per square meter of heliostat aperture area of site improvements
		_site_cost,		//Site improvements cost
		_heliostat_spec_cost,	//Cost per square meter of heliostat aperture area of the heliostat field
		_heliostat_cost,	//Heliostat field cost
		_wiring_cost,	//Wiring cost
		_wiring_user_spec,	//Cost of wiring per square meter of heliostat aperture area
		_plant_cost,	//Cost of the power block and balance of plant equipment
		_plant_spec_cost,	//Cost of the power block and balance of plant equipment per kilowatt (electric) gross design power
		_tes_cost,		//Thermal storage cost
		_tes_spec_cost,	//Cost of thermal storage per kilowatt hour (thermal) capacity
		_land_cost,		//Land cost
		_land_const,  //fixed land area [acre]
		_land_mult,	  //bounding land area multiplier 
		_land_area_tot,  //calculated total land area [m2]
		_land_spec_cost,	//Cost of land per acre including the footprint of the land occupied by the entire plant.
		_fixed_cost,	//Cost that does not scale with any plant parameter
		_contingency_rate,	//Fraction of the direct capital costs added to account for contingency
		_contingency_cost,	//Contingency cost
		_sales_tax_rate,	//Sales tax rate applid to the total direct capital cost
		_sales_tax_frac,	//Fraction of the direct capital costs for which sales tax applies
		_sales_tax_cost,	//Sales tax cost
		_total_direct_cost,		//Sum of all direct costs
		_total_indirect_cost,	//Sum of all indirect costs
		_total_installed_cost,	//Sum of direct and indirect costs
		_cost_per_capacity,		//Estimated capital cost per capacity (net)
		_pmt_factor_1, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_2, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_3, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_4, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_5, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_6, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_7, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_8, 		//Relative value of electricity produced during this period compared to the average
		_pmt_factor_9;		//Relative value of electricity produced during this period compared to the average

	double
		_all_pmt_factors[9];

	double
		_simple_coe,
		_weighted_coe;

	bool
		_is_pmt_factors;		//Enable or disable the use of weighting factors in determining field layout
	
	string
		_weekday_sched,		//Weekday dispatch period schedule
		_weekend_sched;		//Weekend dispatch period schedule
	matrix_t<int>
		_schedule_array;	//8760 schedule of dispatch period
		

public:
	 Financial (){}; 
	 ~Financial (){};

	 void Create(var_map &V);
	void Clean();
	void CreateHourlyTODSchedule(string &wd_schedule, string &we_schedule, matrix_t<int> &hr_array);
	
	
	
	matrix_t<int> *getHourlyTODSchedule(); //8760 TOD matrix
	double *getPaymentFactors();
	double getTowerCost();
	double getReceiverCost();
	double getSiteCost();
	double getHeliostatCost();
	double getWiringCost();
	double getPlantCost();
	double getTESCost();
	double getLandCost();
	double getFixedCost();
	double getContingencyCost();
	double getSalesTaxCost();
	double getTotalDirectCost();
	double getTotalIndirectCost();
	double getTotalInstalledCost();
	double getCostPerCapacity();
	double getLandAreaTot();
	
	bool isPaymentFactors();
	void isPaymentFactors(bool value);
	void calcPlantCapitalCost(SolarField &SF);
	void calcSimpleCOE(double *enet /*8760 MWh*/, int nval = 8760); //Calculates _simple_coe and _weighted_coe


 } ;

#endif