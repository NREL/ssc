#include "lib_power_electronics.h"

dc_connected_battery_controller::dc_connected_battery_controller(dispatch_t * dispatch, float dc_dc_efficiency, float ac_dc_efficiency) : 
charge_controller(dispatch, dc_dc_efficiency, ac_dc_efficiency)
{
	_dc_dc_charge_controller = new dc_dc_charge_controller(dc_dc_efficiency);
	_rectifier = new rectifier(ac_dc_efficiency);
}

ac_connected_battery_controller::ac_connected_battery_controller(dispatch_t * dispatch, float dc_ac_efficiency, float ac_dc_efficiency) :
charge_controller(dispatch, dc_ac_efficiency, ac_dc_efficiency)
{
	_bidirectional_inverter = new bidirectional_inverter(dc_ac_efficiency, ac_dc_efficiency);
}

