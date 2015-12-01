#include "Plant.h"
#include "math.h"
#include <vector>
using namespace std;

//
double Plant::getPlantGrossPower()
{
    return _power_gross;
}

double Plant::getPlantNetPower()
{
    return _power_net;
}

double Plant::getTESThermalCapacity()
{
    return _power_gross / _eta_cycle * _hours_tes;
}

double Plant::getCycleEfficiency()
{
    return _eta_cycle;
}

double Plant::getGrossToNetFactor()
{
    return _par_factor;
}

void Plant::Create(var_map &V)
{
	setVar("eta_cycle", _eta_cycle, V, 0.425, "(0.,1]");		//Thermodynamic efficiency of the power cycle, including feedwater pumps and cooling equipment parasitics
	setVar("solar_mult", _solar_mult, V, 1.9, "(0.,100]");		//Ratio of thermal power output from the solar field to power cycle thermal input at design
	setVar("power_net", _power_net, V, 100., "(0,9e9]");		//Estimated net electric power at design, accounting for all parasitic losses
	setVar("power_gross", _power_gross, V, 115., "(0,9e9]");		//Rated nameplate design gross turbine electric output, not accounting for parasitic losses
	setVar("par_factor", _par_factor, V, 0.87, "(0.,1]");		//Estimated ratio of net power output to gross power output at design
	setVar("power_net_min", _power_net_min, V, 100., "[0,9e9]");		//Minimum net electric power rating considered for optimization - calculated from Layout page
	setVar("power_net_max", _power_net_max, V, 100., "[0,9e9]");		//Maximum net electric power rating considered for optimization - calculated from Layout page
	setVar("hours_tes", _hours_tes, V, 6., "[0.,100]");		//Capacity of Hours of thermal storage operation at full cycle output
}

void Plant::Clean(){
	/* Remove key variables that shouldn't be available when a new solar field is created. */

	//Nothing here yet.

	return;
}