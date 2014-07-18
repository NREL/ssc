#ifndef _PLANT_H_
#define _PLANT_H_ 1
#include "math.h"
#include "mod_base.h"
#include "Toolbox.h"
#include <vector>
using namespace std;

class Plant : public mod_base
 {
	//variable definitions
	double
		_power_net,		//Estimated net electric power at design, accounting for all parasitic losses
		_power_gross,	//Rated nameplate design gross turbine electric output, not accounting for parasitic losses
		_par_factor, 		//Estimated ratio of net power output to gross power output at design
		_eta_cycle, 		//Thermodynamic efficiency of the power cycle, including feedwater pumps and cooling equipment parasitics
		_solar_mult,	//Ratio of thermal power output from the solar field to power cycle thermal input at design
		_hours_tes, 	//Capacity of Hours of thermal storage operation at full cycle output
		_power_net_min, //Minimum net electric power rating considered for optimization - calculated from Layout page
		_power_net_max;		//Maximum net electric power rating considered for optimization - calculated from Layout page
	
 public:
	 /*Plant (){}; 
	 ~Plant (){};*/
	void Create(var_map &V); //Create from variable map
	void Clean();

	double getPlantGrossPower();
	double getPlantNetPower();
	double getTESThermalCapacity();
	double getCycleEfficiency();
	double getGrossToNetFactor();

 } ;

#endif