#include "lib_power_electronics.h"
#include "lib_sandia.h"
#include <cmath>

double bidirectional_inverter::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}
double bidirectional_inverter::convert_to_ac(double P_dc, double * P_ac)
{
	double P_loss = P_dc * (1 - _dc_ac_efficiency);
	*P_ac = P_dc * _dc_ac_efficiency;
	return P_loss;
}
double bidirectional_inverter::compute_dc_from_ac(double P_ac)
{
	return P_ac / _dc_ac_efficiency;
}

double rectifier::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}

double dc_dc_charge_controller::convert_dc_to_dc(double P_dc_in, double * P_dc_out)
{
	double P_loss = P_dc_in * (1 - _dc_dc_efficiency);
	*P_dc_out = P_dc_in * _dc_dc_efficiency;
	return P_loss;
}

charge_controller::charge_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiency_1, double efficiency_2)
{	
	_dispatch = dispatch;
	_battery_metrics = battery_metrics;
	initialize(0,0);
}
void charge_controller::initialize(double P_pv, double P_load_ac)
{
	_P_load = P_load_ac;
	_P_grid = 0;
	_P_grid_to_load = 0;
	_P_grid_to_batt = 0;
	_P_gen = 0;
	_P_battery_to_load = 0;
	_P_pv_to_load = 0;
	_P_pv_to_battery = 0;
	_P_pv_to_grid = 0;
	_P_battery = 0;

	// ac for ac-connected, dc for dc-connected
	_P_pv = P_pv;
}

double charge_controller::grid_ac(double dc_ac_efficiency)
{
	// dc powers
	double P_grid_to_batt_0 = _dispatch->power_grid_to_batt();
	double P_grid_to_load_0 = _dispatch->power_grid_to_load();

	// ac grid power required to meet dc charging
	if (_P_battery < 0)
		_P_grid_to_batt = fabs(_P_battery + _P_pv_to_battery);
	double P_grid_loss = (_P_grid_to_batt - P_grid_to_batt_0);

	// reconcile any conversion with required load 
	// P_grid < 0 -> using grid; _P_grid > 0 -> exporting to grid
	_P_grid_to_load = _P_load - _P_pv_to_load - _P_battery_to_load;
	_P_grid = -_P_grid_to_load - _P_grid_to_batt + _P_pv_to_grid;
	
	if (_P_grid_to_load < 0)
		_P_grid_to_load = 0.;
	if (_P_grid_to_batt < 0)
		_P_grid_to_batt = 0.;
		
	return P_grid_loss; 
}


dc_connected_battery_controller::dc_connected_battery_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double dc_dc_efficiency, double inverter_efficiency) :
charge_controller(dispatch, battery_metrics, 100, dc_dc_efficiency)
{
	_dc_dc_charge_controller = new dc_dc_charge_controller(dc_dc_efficiency);
	_inverter_efficiency = inverter_efficiency * 0.01;
}
dc_connected_battery_controller::~dc_connected_battery_controller()
{
	if (_dc_dc_charge_controller)
		delete _dc_dc_charge_controller;
}

double dc_connected_battery_controller::run(size_t year, size_t hour_of_year, size_t step_of_hour, double P_pv_dc, double P_load_ac)
{
	double P_pv_dc_converted;
	
	initialize(P_pv_dc, P_load_ac);

	// derate PV power being passed to battery
	double P_loss_pv_dc = _dc_dc_charge_controller->convert_dc_to_dc(P_pv_dc, &P_pv_dc_converted);

	// compute what dc load would have to be to match load, i.e
	double P_load_dc =  (1./_inverter_efficiency) * P_load_ac;

	// dispatch battery, load must be in dc.  This computes dc battery power to load, dc pv power to load, dc grid power to dc load, dc grid power to battery
	_dispatch->dispatch(year, hour_of_year, step_of_hour, P_pv_dc_converted, P_load_dc);

	double P_loss_gen_dc = gen_dc();	
	double P_loss_gen_ac = gen_ac();
	double P_loss_grid = grid_ac();

	// AC charging metrics
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);

	return P_loss_grid + P_loss_gen_dc + P_loss_gen_ac;
}

double dc_connected_battery_controller::grid_ac()
{
	return charge_controller::grid_ac(_inverter_efficiency);
}
double dc_connected_battery_controller::gen_dc()
{
	// derate battery power being discharged through charge_controller 
	double P_gen_dc = _dispatch->power_gen();
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_to_load_dc = _dispatch->power_battery_to_load();
	double P_pv_to_load_dc = _dispatch->power_pv_to_load();
	double P_pv_to_battery_dc = _dispatch->power_pv_to_batt();
	double P_pv_to_grid_dc = _dispatch->power_pv_to_grid();

	// apply conversion losses, power is still DC
	double P_loss_gen = _dc_dc_charge_controller->convert_dc_to_dc(P_gen_dc, &_P_gen);
	double P_loss_battery = _dc_dc_charge_controller->convert_dc_to_dc(P_battery_dc, &_P_battery);
	double P_loss_battery_to_load = _dc_dc_charge_controller->convert_dc_to_dc(P_battery_to_load_dc, &_P_battery_to_load);
	double P_loss_pv_to_load = _dc_dc_charge_controller->convert_dc_to_dc(P_pv_to_load_dc, &_P_pv_to_load);
	double P_loss_pv_to_battery = _dc_dc_charge_controller->convert_dc_to_dc(P_pv_to_battery_dc, &_P_pv_to_battery);
	double P_loss_pv_to_grid = _dc_dc_charge_controller->convert_dc_to_dc(P_pv_to_grid_dc, &_P_pv_to_grid);

	// compute total loss due to conversions
	return  P_loss_gen;
}

double dc_connected_battery_controller::gen_ac()
{
	// dc quantities
	double P_battery_dc = _P_battery;
	double P_battery_to_load_dc = _P_battery_to_load;
	double P_pv_to_load_dc = _P_pv_to_load;
	double P_pv_to_battery_dc = _P_pv_to_battery;
	double P_pv_to_grid_dc = _P_pv_to_grid;

	// ac quantities
	_P_battery_to_load = P_battery_to_load_dc * _inverter_efficiency;
	_P_pv_to_load = P_pv_to_load_dc * _inverter_efficiency;
	_P_pv_to_grid = P_pv_to_grid_dc * _inverter_efficiency;

	if (P_battery_dc > 0)
	{
		_P_battery = P_battery_dc * _inverter_efficiency;
		_P_pv_to_battery = P_pv_to_battery_dc * _inverter_efficiency;
	}
	else if (P_battery_dc < 0)
	{
		_P_battery = P_battery_dc / _inverter_efficiency;
		_P_pv_to_battery = P_pv_to_battery_dc / _inverter_efficiency;

		if (_P_pv_to_battery > _P_pv)
		{
			double dPv = _P_pv_to_battery - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
		else if (_P_pv_to_battery + _P_pv_to_load > _P_pv)
		{
			double dPv = _P_pv_to_battery + _P_pv_to_load - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
	}

	// extra metrics if desired
	double P_loss_battery_to_load = P_battery_to_load_dc - _P_battery_to_load;
	double P_loss_pv_to_load = P_pv_to_load_dc - _P_pv_to_load;
	double P_loss_pv_to_battery = P_pv_to_battery_dc - _P_pv_to_battery;
	double P_loss_pv_to_grid = P_pv_to_grid_dc - _P_pv_to_grid;
	double P_loss_battery = P_battery_dc - _P_battery;

	return 0.;
}
double dc_connected_battery_controller::update_gen_ac(double P_gen_ac)
{
	double P_loss_gen = _P_gen - P_gen_ac;
	_P_gen = P_gen_ac;
	return P_loss_gen;
}

ac_connected_battery_controller::ac_connected_battery_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double ac_dc_efficiency, double dc_ac_efficiency) :
charge_controller(dispatch, battery_metrics, ac_dc_efficiency, dc_ac_efficiency)
{
	_bidirectional_inverter = new bidirectional_inverter(ac_dc_efficiency, dc_ac_efficiency);
}
ac_connected_battery_controller::~ac_connected_battery_controller()
{
	if (_bidirectional_inverter)
		delete _bidirectional_inverter;
}
double ac_connected_battery_controller::run( size_t year, size_t hour_of_year, size_t step_of_hour, double P_pv_ac, double P_load_ac)
{
	double P_pv_dc = 0;
	
	initialize(P_pv_ac, P_load_ac);

	// derate pv power passed to battery
	double P_loss_pv_ac = _bidirectional_inverter->convert_to_dc(P_pv_ac, &P_pv_dc);

	// compute what dc load would have to be to match load
	double P_load_dc = _bidirectional_inverter->compute_dc_from_ac(P_load_ac);

	_dispatch->dispatch(year, hour_of_year, step_of_hour, P_pv_dc, P_load_dc);

	// Apply conversion losses on gen
	double P_loss_gen = gen_ac();

	// compute ac grid power required to achieve dc grid power computed
	double P_loss_grid = grid_ac();

	// AC charging metrics
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);

	// compute loss
	return P_loss_grid + P_loss_gen;
}
double ac_connected_battery_controller::grid_ac()
{
	double P_loss_grid = charge_controller::grid_ac(_bidirectional_inverter->ac_dc_efficiency());
	return P_loss_grid;
}
double ac_connected_battery_controller::gen_ac()
{
	// dc quantities
	double P_gen_dc = _dispatch->power_gen();
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_to_load_dc = _dispatch->power_battery_to_load();
	double P_pv_to_load_dc = _dispatch->power_pv_to_load();
	double P_pv_to_battery_dc = _dispatch->power_pv_to_batt();

	// ac quantities
	_P_gen = P_gen_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_battery_to_load = P_battery_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_pv_to_load = P_pv_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();

	if (P_battery_dc > 0)
	{
		_P_battery = P_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
	}
	else if (P_battery_dc < 0)
	{
		_P_battery = P_battery_dc / _bidirectional_inverter->ac_dc_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc / _bidirectional_inverter->ac_dc_efficiency();

		if (_P_pv_to_battery > _P_pv)
		{
			double dPv = _P_pv_to_battery - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
		else if (_P_pv_to_battery + _P_pv_to_load > _P_pv)
		{
			double dPv = _P_pv_to_battery + _P_pv_to_load - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
	}
	double P_loss_gen = P_gen_dc - _P_gen;

	// extra metrics if desired
	double P_loss_battery_to_load = P_battery_to_load_dc - _P_battery_to_load;
	double P_loss_pv_to_load = P_pv_to_load_dc - _P_pv_to_load;
	double P_loss_pv_to_battery = P_pv_to_battery_dc - _P_pv_to_battery;
	double P_loss_battery = P_battery_dc - _P_battery;


	return P_loss_gen;
}
