#ifndef _power_electronics_h_
#define _power_electronics_h_

#include "lib_battery.h"

class bidirectional_inverter
{
public:
	
	bidirectional_inverter(float dc_ac_efficiency, float ac_dc_efficiency)
	{
		_dc_ac_efficiency = dc_ac_efficiency;
		_ac_dc_efficiency = ac_dc_efficiency;
	}

	// return power loss [kW]
	double convert_to_dc(double * input);
	double convert_to_ac(double * input);

protected:
	float _dc_ac_efficiency;
	float _ac_dc_efficiency;
};

class dc_dc_charge_controller
{
public:
	
	dc_dc_charge_controller(float dc_dc_efficiency){ _dc_dc_efficiency = dc_dc_efficiency; }

	// return power loss [kW]
	double convert_dc_to_dc(double *input);

protected:
	float _dc_dc_efficiency;

};

class rectifier
{
public:

	rectifier(float dc_ac_efficiency){_dc_ac_efficiency = dc_ac_efficiency;}
	
	// return power loss [kW]
	double convert_to_dc(double *input);

protected:
	float _dc_ac_efficiency;
};

class charge_controller
{
public:
	charge_controller(dispatch_t * dispatch, double efficiency_1, double efficiency_2){};

	// return power loss [kW]
	virtual double run_battery(double P_pv, double P_load, double * P_grid, double * P_gen) = 0;


	void accumulate_charge_energy();
	void accumulate_discharge_energy();
	void accumulate_conversion_losses();

	enum {AC_CONNECTED, DC_CONNECTED};

protected:

	double _round_trip_efficiency;
	dispatch_t * _dispatch;


};

class dc_connected_battery_controller : public charge_controller
{
public:
	dc_connected_battery_controller(dispatch_t * dispatch, float dc_dc_efficiency, float ac_dc_efficiency);
	double run_battery(double P_pv_dc, double P_load_ac, double * P_grid_ac, double * P_gen_dc);

protected:
	dc_dc_charge_controller * _dc_dc_charge_controller;
	rectifier * _rectifier;
};

class ac_connected_battery_controller : public charge_controller
{
public:
	ac_connected_battery_controller(dispatch_t * dispatch, float ac_dc_efficiency, float dc_ac_efficiency);

	double run_battery(double P_pv_ac, double P_load_ac, double * P_grid_ac, double * P_gen_ac);

protected:
	bidirectional_inverter * _bidirectional_inverter;
};

#endif